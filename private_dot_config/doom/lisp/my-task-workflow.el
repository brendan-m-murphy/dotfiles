;;; my-task-workflow.el --- Task-centric Org workflow helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Minimal task workflow for Org planning, GitHub issues, git worktrees, and
;; Doom workspaces. A task is anchored at a configurable Org heading level.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-md)
(require 'subr-x)

(declare-function +workspace/new "ext:doom-workspaces" (name))
(declare-function +workspace/switch-to "ext:doom-workspaces" (name &optional auto-create-p))
(declare-function magit-status "ext:magit-status" (&optional directory cache))
(declare-function magit-status-setup-buffer "ext:magit-status" (directory))
(declare-function my/gptel-add-session-allowed-roots "my-gptel-org-workflow" (roots))

(defgroup my/task-workflow nil
  "Task-centric Org workflow helpers."
  :group 'tools)

(defcustom my/task-heading-level 2
  "Org heading level used as the task root."
  :type 'integer
  :group 'my/task-workflow)

(defcustom my/task-default-base-branches '("main" "devel" "develop" "master")
  "Branch names accepted as safe bases for new task branches.
A task or file can override this with DEFAULT_BASE_BRANCH."
  :type '(repeat string)
  :group 'my/task-workflow)

(defcustom my/task-report-states '("TODO" "PROJ" "WAIT" "HOLD")
  "TODO states included in the active repo-task report."
  :type '(repeat string)
  :group 'my/task-workflow)

(defconst my/task-properties
  '("REPO_PATH"
    "DEFAULT_BASE_BRANCH"
    "GH_REPO"
    "GH_ISSUE"
    "GH_URL"
    "PR"
    "BRANCH"
    "WORKTREE"
    "WORKSPACE"
    "GPTEL_TOPIC"
    "NEXT_ACTION"
    "CREATED"
    "LAST_REVIEWED")
  "Task properties managed by the task workflow.")

(defun my/task--task-heading-level-p (&optional level)
  "Return non-nil when LEVEL is the configured task heading level."
  (= (or level (org-current-level) 0) my/task-heading-level))

(defun my/task--task-root-point ()
  "Return the point of the current task root.
The task root is the nearest ancestor heading at `my/task-heading-level'."
  (unless (derived-mode-p 'org-mode)
    (user-error "Task commands require Org mode"))
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t))
    (unless (org-at-heading-p)
      (user-error "Point is not inside an Org task subtree"))
    (let ((found nil))
      (while (and (not found) (org-at-heading-p))
        (if (my/task--task-heading-level-p)
            (setq found (point))
          (unless (org-up-heading-safe)
            (setq found :missing))))
      (if (eq found :missing)
          (user-error "No task heading found at level %d" my/task-heading-level)
        found))))

(defun my/task--goto-task-root ()
  "Move point to the current task root and return point."
  (goto-char (my/task--task-root-point)))

(defun my/task--heading-title ()
  "Return the current heading title without TODO/priority/tags."
  (org-get-heading t t t t))

(defun my/task--slugify (title)
  "Return a slug derived from TITLE."
  (let* ((title (downcase title))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" title)))
    (string-trim slug "-" "-")))

(defun my/task--heading-slug ()
  "Return a slug derived from the current task heading."
  (my/task--slugify (my/task--heading-title)))

(defun my/task--org-timestamp-now ()
  "Return the current time as an inactive Org timestamp string."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun my/task--get-property (property &optional inherit)
  "Return PROPERTY from the current task heading.
When INHERIT is non-nil, allow Org property inheritance."
  (when-let ((value (org-entry-get nil property inherit)))
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun my/task--file-property (property)
  "Return file-level Org PROPERTY keyword value, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward
           (format "^[ \t]*#\\+PROPERTY:[ \t]+%s[ \t]+\\(.+\\)$"
                   (regexp-quote property))
           nil t)
      (string-trim (match-string-no-properties 1)))))

(defun my/task--get-property-or-file (property &optional inherit)
  "Return PROPERTY from the current heading, inherited headings, or file keyword."
  (or (my/task--get-property property inherit)
      (my/task--file-property property)))

(defun my/task--set-property (property value)
  "Set PROPERTY to VALUE on the current task heading."
  (when (and value (not (string-empty-p value)))
    (let ((current (my/task--get-property property nil)))
      (unless (equal current value)
        (org-entry-put nil property value)))))

(defun my/task--write-properties (plist)
  "Write managed task properties from PLIST onto the current task heading."
  (dolist (property my/task-properties)
    (my/task--set-property property (plist-get plist (intern (concat ":" (downcase property)))))))

(defun my/task--call-process-string (program &rest args)
  "Run PROGRAM with ARGS and return trimmed stdout.
Signal `user-error' on non-zero exit."
  (with-temp-buffer
    (let ((exit-code (apply #'process-file program nil (current-buffer) nil args))
          (output nil))
      (setq output (string-trim (buffer-string)))
      (unless (zerop exit-code)
        (user-error "Command failed: %s %s\n%s"
                    program
                    (string-join args " ")
                    output))
      output)))

(defun my/task--git-root (path)
  "Return the git root for PATH, or nil if PATH is not in a git repo."
  (let ((default-directory (file-name-as-directory (expand-file-name path))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil (current-buffer) nil
                                 "rev-parse" "--show-toplevel"))
        (file-name-as-directory
         (file-truename (string-trim (buffer-string))))))))

(defun my/task--repo-path ()
  "Resolve the current task repo path, prompting if necessary.
The stored value is always normalized to the repo root."
  (or
   (when-let ((repo-path (or (my/task--get-property "REPO_PATH" t)
                             (my/task--file-property "REPO_PATH"))))
     (let ((expanded (expand-file-name repo-path)))
       (when (file-remote-p expanded)
         (user-error "Remote REPO_PATH is not supported: %s" repo-path))
       (unless (file-directory-p expanded)
         (user-error "REPO_PATH does not exist: %s" repo-path))
       (or (my/task--git-root expanded)
           (user-error "REPO_PATH is not inside a git repo: %s" repo-path))))
   (let* ((selected (read-directory-name "Repo path: " nil nil t))
          (git-root (my/task--git-root selected)))
     (unless git-root
       (user-error "Selected path is not inside a git repo: %s" selected))
     (my/task--set-property "REPO_PATH" git-root)
     git-root)))

(defun my/task--repo-name (repo-path)
  "Return the repo directory name for REPO-PATH."
  (file-name-nondirectory (directory-file-name repo-path)))

(defun my/task--worktree-root (repo-path)
  "Return the dedicated worktree root for REPO-PATH."
  (expand-file-name
   (concat (my/task--repo-name repo-path) "-wt")
   (file-name-directory (directory-file-name repo-path))))

(defun my/task--find-child-heading (title)
  "Return point of the direct child heading named TITLE, or nil."
  (save-excursion
    (my/task--goto-task-root)
    (let ((task-level (org-current-level))
          (task-end (save-excursion (org-end-of-subtree t t)))
          (found nil))
      (forward-line 1)
      (while (and (not found)
                  (< (point) task-end)
                  (re-search-forward org-heading-regexp task-end t))
        (let ((level (length (match-string 1))))
          (cond
           ((<= level task-level)
            (goto-char task-end))
           ((and (= level (1+ task-level))
                 (string-equal (org-get-heading t t t t) title))
            (setq found (line-beginning-position))))))
      found)))

(defun my/task--notes-subtree-marker ()
  "Return a marker for the direct `Notes' child subtree."
  (or (when-let ((point (my/task--find-child-heading "Notes")))
        (copy-marker point))
      (user-error "Task must contain a %s Notes section to create a GitHub issue"
                  (make-string (1+ my/task-heading-level) ?*))))

(defun my/task--export-notes-to-markdown ()
  "Export the task's `Notes' subtree to Markdown and return it as a string."
  (let ((marker (my/task--notes-subtree-marker)))
    (unwind-protect
        (save-excursion
          (goto-char marker)
          (org-back-to-heading t)
          (let ((content (save-restriction
                           (org-narrow-to-subtree)
                           (buffer-substring-no-properties (point-min) (point-max)))))
            (org-export-string-as content 'md t '(:with-toc nil :section-numbers nil))))
      (set-marker marker nil))))

(defun my/task--prompt-issue-title ()
  "Prompt for the issue title, defaulting to the task heading."
  (let ((default (my/task--heading-title)))
    (read-string (format "Issue title (%s): " default) nil nil default)))

(defun my/task--github-issue-url (repo issue)
  "Return the GitHub issue URL for REPO and ISSUE."
  (format "https://github.com/%s/issues/%s" repo issue))

(defun my/task--parse-github-issue-url (url)
  "Parse a GitHub issue URL into a plist, or nil if URL is unsupported."
  (when (string-match
         "\\`https?://\\(?:www\\.\\)?github\\.com/\\([^/]+\\)/\\([^/]+\\)/issues/\\([0-9]+\\)/?\\'"
         (string-trim url))
    (let* ((trimmed (string-trim url))
           (repo (format "%s/%s" (match-string 1 trimmed) (match-string 2 trimmed)))
           (issue (match-string 3 trimmed)))
      (list :gh_repo repo
            :gh_issue issue
            :gh_url (my/task--github-issue-url repo issue)))))

(defun my/task--repo-from-git-url (url)
  "Return owner/name parsed from GitHub remote URL, or nil."
  (let ((trimmed (string-trim url)))
    (cond
     ((string-match "\\`git@github\\.com:\\([^/]+\\)/\\(.+\\)\\'" trimmed)
      (format "%s/%s" (match-string 1 trimmed)
              (replace-regexp-in-string "\\.git\\'" "" (match-string 2 trimmed))))
     ((string-match "\\`ssh://git@github\\.com/\\([^/]+\\)/\\(.+\\)\\'" trimmed)
      (format "%s/%s" (match-string 1 trimmed)
              (replace-regexp-in-string "\\.git\\'" "" (match-string 2 trimmed))))
     ((string-match "\\`https?://\\(?:[^/@]+@\\)?github\\.com/\\([^/]+\\)/\\(.+\\)\\'" trimmed)
      (format "%s/%s" (match-string 1 trimmed)
              (replace-regexp-in-string "\\.git\\'" ""
                                        (replace-regexp-in-string "/\\'" "" (match-string 2 trimmed))))))))

(defun my/task--gh-repo-from-gh (repo-path)
  "Return owner/name for REPO-PATH using `gh repo view', or nil."
  (when (and repo-path (executable-find "gh"))
    (let ((default-directory repo-path))
      (with-temp-buffer
        (when (zerop (process-file "gh" nil (current-buffer) nil
                                   "repo" "view" "--json" "nameWithOwner"
                                   "--jq" ".nameWithOwner"))
          (let ((value (string-trim (buffer-string))))
            (unless (string-empty-p value)
              value)))))))

(defun my/task--gh-repo-from-remote (repo-path)
  "Return owner/name for REPO-PATH using git remote.origin.url, or nil."
  (when repo-path
    (let ((default-directory repo-path))
      (with-temp-buffer
        (when (zerop (process-file "git" nil (current-buffer) nil
                                   "config" "--get" "remote.origin.url"))
          (my/task--repo-from-git-url (buffer-string)))))))

(defun my/task--repo-path-from-properties-no-prompt ()
  "Return a task repo path from Org properties without prompting."
  (when-let ((repo-path (or (my/task--get-property "REPO_PATH" t)
                            (my/task--file-property "REPO_PATH"))))
    (let ((expanded (expand-file-name repo-path)))
      (unless (file-remote-p expanded)
        (if (file-directory-p expanded)
            (or (my/task--git-root expanded)
                (file-name-as-directory (file-truename expanded)))
          (file-name-as-directory expanded))))))

(defun my/task--resolve-gh-repo (&optional repo-path)
  "Resolve the current task's GitHub owner/name repository.
Resolution order is direct/inherited/file `GH_REPO', `GH_URL', `gh repo view',
then the git origin remote."
  (or (my/task--get-property-or-file "GH_REPO" t)
      (when-let* ((url (my/task--get-property "GH_URL" t))
                  (parsed (my/task--parse-github-issue-url url)))
        (plist-get parsed :gh_repo))
      (my/task--gh-repo-from-gh repo-path)
      (my/task--gh-repo-from-remote repo-path)))

(defun my/task--normalize-issue-reference (issue-ref &optional gh-repo)
  "Normalize ISSUE-REF into a plist with repo, issue number, and URL.
ISSUE-REF may be a bare number or a full GitHub issue URL.  Bare numbers need
GH-REPO so the task can store an unambiguous `GH_URL'."
  (let ((trimmed (string-trim issue-ref)))
    (cond
     ((string-match "\\`[0-9]+\\'" trimmed)
      (unless gh-repo
        (user-error "A bare issue number needs GH_REPO; set GH_REPO or REPO_PATH first"))
      (list :gh_repo gh-repo
            :gh_issue trimmed
            :gh_url (my/task--github-issue-url gh-repo trimmed)))
     ((my/task--parse-github-issue-url trimmed))
     (t
      (user-error "Issue reference must be a number or GitHub issue URL: %s"
                  issue-ref)))))

(defun my/task--prompt-issue-reference ()
  "Prompt for an issue number or URL."
  (read-string "Issue number or URL: "
               nil nil
               (or (my/task--get-property "GH_URL" nil)
                   (my/task--get-property "GH_ISSUE" nil)
                   "")))

(defun my/task--derive-workspace (repo-name issue slug)
  "Derive a workspace name from REPO-NAME, ISSUE, and SLUG."
  (if issue
      (format "%s-iss-%s" repo-name issue)
    (format "%s-task-%s" repo-name slug)))

(defun my/task--derive-branch (issue slug)
  "Derive a branch name from ISSUE and SLUG."
  (if issue
      (format "issue-%s-%s" issue slug)
    slug))

(defun my/task--branch-slug (branch)
  "Return a slug-like value derived from BRANCH."
  (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" branch))))
    (string-trim slug "-" "-")))

(defun my/task--derive-worktree (repo-path workspace)
  "Derive a worktree path from REPO-PATH and WORKSPACE."
  (expand-file-name workspace (my/task--worktree-root repo-path)))

(defun my/task--current-branch (repo-path)
  "Return the current branch name in REPO-PATH."
  (let ((default-directory repo-path))
    (my/task--call-process-string "git" "branch" "--show-current")))

(defun my/task--upstream-branch (repo-path branch)
  "Return BRANCH's upstream branch in REPO-PATH, or nil if it has none."
  (let ((default-directory repo-path))
    (with-temp-buffer
      (when (zerop (process-file "git" nil (current-buffer) nil
                                 "rev-parse" "--abbrev-ref"
                                 (format "%s@{upstream}" branch)))
        (let ((upstream (string-trim (buffer-string))))
          (unless (string-empty-p upstream)
            upstream))))))

(defun my/task--branch-behind-count (repo-path branch upstream)
  "Return how many commits BRANCH is behind UPSTREAM in REPO-PATH."
  (let* ((default-directory repo-path)
         (output (my/task--call-process-string
                  "git" "rev-list" "--left-right" "--count"
                  (format "%s...%s" branch upstream)))
         (counts (split-string output "[ \t\n]+" t)))
    (string-to-number (or (cadr counts) "0"))))

(defun my/task--assert-base-ready (repo-path base-branch)
  "Signal if REPO-PATH is not on a safe, up-to-date task base branch."
  (let* ((current (my/task--current-branch repo-path))
         (allowed (if base-branch
                      (list base-branch)
                    my/task-default-base-branches)))
    (unless (member current allowed)
      (user-error "Refusing to create task branch from %s; expected one of: %s"
                  (if (string-empty-p current) "detached HEAD" current)
                  (string-join allowed ", ")))
    (if-let ((upstream (my/task--upstream-branch repo-path current)))
        (let ((behind (my/task--branch-behind-count repo-path current upstream)))
          (when (> behind 0)
            (user-error "Refusing to create task branch: %s is %d commits behind %s"
                        current behind upstream)))
      (message "No upstream configured for %s; remote freshness was not checked"
               current))))

(defun my/task--parse-gh-issue-output (output)
  "Parse gh issue creation OUTPUT into a plist."
  (or (my/task--parse-github-issue-url (string-trim output))
      (user-error "Could not parse gh issue output: %s" output)))

(defun my/task--branch-names (repo-path)
  "Return local branch names in REPO-PATH."
  (let ((default-directory repo-path)
        (output nil))
    (setq output (my/task--call-process-string
                  "git" "for-each-ref" "--format=%(refname:short)" "refs/heads"))
    (split-string output "\n" t)))

(defun my/task--prompt-branch (repo-path)
  "Prompt for a branch name in REPO-PATH."
  (let* ((branches (my/task--branch-names repo-path))
         (default (or (my/task--get-property "BRANCH" nil)
                      (car branches)
                      "")))
    (if branches
        (completing-read (format "Branch (%s): " default)
                         branches nil t nil nil default)
      (read-string "Branch: " default))))

(defun my/task--metadata-with-overrides (metadata &rest overrides)
  "Return METADATA plist with OVERRIDES applied."
  (let ((result (copy-sequence metadata)))
    (while overrides
      (setq result (plist-put result (pop overrides) (pop overrides))))
    result))

(defun my/task--refresh-derived-metadata (metadata)
  "Refresh derived metadata fields from METADATA."
  (let* ((issue (plist-get metadata :gh_issue))
         (repo-path (plist-get metadata :repo_path))
         (repo-name (plist-get metadata :repo_name))
         (slug (plist-get metadata :slug))
         (workspace (or (plist-get metadata :workspace_explicit)
                        (plist-get metadata :workspace)
                        (my/task--derive-workspace repo-name issue slug)))
         (branch (or (plist-get metadata :branch_explicit)
                     (plist-get metadata :branch)
                     (my/task--derive-branch issue slug)))
         (worktree (or (plist-get metadata :worktree_explicit)
                       (plist-get metadata :worktree)
                       (my/task--derive-worktree repo-path workspace))))
    (setq metadata (plist-put metadata :workspace workspace))
    (setq metadata (plist-put metadata :branch branch))
    (setq metadata (plist-put metadata :worktree worktree))
    metadata))

(defun my/task--gh-create-issue (repo-path title body-file)
  "Create a GitHub issue in REPO-PATH with TITLE and BODY-FILE."
  (let ((default-directory repo-path))
    (apply #'my/task--parse-gh-issue-output
           (list
            (my/task--call-process-string
             "gh" "issue" "create" "--title" title "--body-file" body-file)))))

(defun my/task--branch-exists-p (repo-path branch)
  "Return non-nil when BRANCH exists locally in REPO-PATH."
  (let ((default-directory repo-path))
    (zerop (process-file "git" nil nil nil
                         "rev-parse" "--verify" (format "refs/heads/%s" branch)))))

(defun my/task--repo-has-worktree-p (repo-path worktree)
  "Return non-nil when WORKTREE is registered under REPO-PATH."
  (let ((default-directory repo-path)
        (target (file-truename worktree)))
    (with-temp-buffer
      (unless (zerop (process-file "git" nil (current-buffer) nil
                                   "worktree" "list" "--porcelain"))
        (user-error "Failed to inspect git worktrees for %s" repo-path))
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^worktree \\(.+\\)$" nil t)
          (when (string-equal (file-truename (match-string 1)) target)
            (throw 'found t)))
        nil))))

(defun my/task--worktree-exists-p (repo-path worktree)
  "Return non-nil when WORKTREE exists and belongs to REPO-PATH."
  (and (file-directory-p worktree)
       (let ((git-root (my/task--git-root worktree)))
         (and git-root
              (string-equal (file-name-as-directory (file-truename worktree)) git-root)
              (my/task--repo-has-worktree-p repo-path worktree)))))

(defun my/task--ensure-worktree (repo-path branch worktree &optional base-branch)
  "Ensure WORKTREE exists for BRANCH in REPO-PATH.
When creating a new branch, require the current repo branch to be an accepted
base branch. BASE-BRANCH overrides `my/task-default-base-branches'."
  (let ((worktree-root (my/task--worktree-root repo-path))
        (default-directory repo-path))
    (make-directory worktree-root t)
    (cond
     ((file-directory-p worktree)
      (unless (my/task--worktree-exists-p repo-path worktree)
        (user-error "Existing WORKTREE is not a valid git worktree for %s: %s"
                    repo-path worktree)))
     ((my/task--branch-exists-p repo-path branch)
      (my/task--call-process-string "git" "worktree" "add" worktree branch))
     (t
      (my/task--assert-base-ready repo-path base-branch)
      (my/task--call-process-string "git" "worktree" "add" "-b" branch worktree "HEAD"))))
  worktree)

(defun my/task--workspace-exists-p (name)
  "Return non-nil if the Doom workspace NAME already exists."
  (and (boundp '+workspace--list)
       (cl-some (lambda (workspace)
                  (string-equal (if (consp workspace) (car workspace) workspace) name))
                +workspace--list)))

(defun my/task--switch-workspace (name)
  "Switch to Doom workspace NAME, creating it if needed."
  (if (my/task--workspace-exists-p name)
      (+workspace/switch-to name)
    (+workspace/new name)
    (+workspace/switch-to name)))

(defun my/task--open-worktree (worktree)
  "Open WORKTREE in Emacs."
  (dired worktree))

(defun my/task--magit-status (worktree)
  "Open Magit status for WORKTREE."
  (if (fboundp 'magit-status-setup-buffer)
      (magit-status-setup-buffer worktree)
    (magit-status worktree)))

(defun my/task--collect-metadata ()
  "Collect current task metadata into a plist."
  (let* ((repo-path (my/task--repo-path))
         (repo-name (my/task--repo-name repo-path))
         (issue (my/task--get-property "GH_ISSUE" nil))
         (gh-repo (my/task--resolve-gh-repo repo-path))
         (slug (my/task--heading-slug))
         (gptel-topic (my/task--get-property "GPTEL_TOPIC" nil))
         (created (my/task--get-property "CREATED" nil))
         (workspace-prop (my/task--get-property "WORKSPACE" nil))
         (branch-prop (my/task--get-property "BRANCH" nil))
         (worktree-prop (my/task--get-property "WORKTREE" nil))
         (workspace (or workspace-prop
                        (my/task--derive-workspace repo-name issue slug)))
         (branch (or branch-prop
                     (my/task--derive-branch issue slug)))
         (worktree (or worktree-prop
                       (my/task--derive-worktree repo-path workspace))))
    (list :repo_path repo-path
          :default_base_branch (my/task--get-property-or-file "DEFAULT_BASE_BRANCH" t)
          :repo_name repo-name
          :gh_repo gh-repo
          :gh_issue issue
          :gh_url (my/task--get-property "GH_URL" nil)
          :pr (my/task--get-property "PR" nil)
          :slug slug
          :gptel_topic (or gptel-topic slug)
          :next_action (my/task--get-property "NEXT_ACTION" nil)
          :created (or created (my/task--org-timestamp-now))
          :last_reviewed (my/task--get-property "LAST_REVIEWED" nil)
          :workspace_explicit workspace-prop
          :branch_explicit branch-prop
          :worktree_explicit worktree-prop
          :branch branch
          :worktree worktree
          :workspace workspace)))

(defun my/task--write-derived-properties (metadata)
  "Write METADATA plist back to the current task heading."
  (my/task--write-properties metadata))

(defun my/task--open-with-metadata (metadata)
  "Open task context using METADATA."
  (let ((repo-path (plist-get metadata :repo_path))
        (branch (plist-get metadata :branch))
        (worktree (plist-get metadata :worktree))
        (workspace (plist-get metadata :workspace))
        (base-branch (plist-get metadata :default_base_branch)))
    (my/task--ensure-worktree repo-path branch worktree base-branch)
    (my/task--write-derived-properties metadata)
    (my/task--switch-workspace workspace)
    (my/task--open-worktree worktree)
    (my/task--magit-status worktree)))

(defun my/task--derived-property-values ()
  "Return derived task property values without prompting or creating worktrees."
  (let* ((repo-path (my/task--repo-path-from-properties-no-prompt))
         (repo-name (and repo-path (my/task--repo-name repo-path)))
         (issue (my/task--get-property "GH_ISSUE" t))
         (gh-repo (my/task--resolve-gh-repo repo-path))
         (slug (my/task--heading-slug))
         (workspace (and repo-name (my/task--derive-workspace repo-name issue slug)))
         (branch (my/task--derive-branch issue slug))
         (worktree (and repo-path workspace
                        (my/task--derive-worktree repo-path workspace))))
    (list (cons "REPO_PATH" repo-path)
          (cons "GH_REPO" gh-repo)
          (cons "GH_URL" (and gh-repo issue (my/task--github-issue-url gh-repo issue)))
          (cons "BRANCH" branch)
          (cons "WORKTREE" worktree)
          (cons "WORKSPACE" workspace)
          (cons "GPTEL_TOPIC" slug))))

(defun my/task--property-snapshot ()
  "Return managed property rows for the current task."
  (save-excursion
    (my/task--goto-task-root)
    (let ((derived (my/task--derived-property-values)))
      (mapcar
       (lambda (property)
         (list :property property
               :direct (my/task--get-property property nil)
               :inherited (my/task--get-property property t)
               :file (my/task--file-property property)
               :derived (cdr (assoc property derived))))
       my/task-properties))))

(defun my/task--read-managed-property (&optional prompt)
  "Read a managed task property using PROMPT."
  (completing-read (or prompt "Property: ") my/task-properties nil t))

;;;###autoload
(defun my/task-describe-properties ()
  "Show direct, inherited, file-level, and derived task property values."
  (interactive)
  (let ((rows (my/task--property-snapshot)))
    (with-current-buffer (get-buffer-create "*Org Task Properties*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Org task properties\n\n")
        (dolist (row rows)
          (insert (format "%s\n" (plist-get row :property)))
          (dolist (field '(:direct :inherited :file :derived))
            (insert (format "  %-9s %s\n"
                            (substring (symbol-name field) 1)
                            (or (plist-get row field) ""))))
          (insert "\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer "*Org Task Properties*")))

;;;###autoload
(defun my/task-set-property (property value)
  "Set managed task PROPERTY to VALUE on the current task heading."
  (interactive
   (let* ((property (my/task--read-managed-property "Set property: "))
          (default (save-excursion
                     (my/task--goto-task-root)
                     (or (my/task--get-property property nil)
                         (cdr (assoc property (my/task--derived-property-values)))))))
     (list property (read-string (format "%s: " property) nil nil default))))
  (save-excursion
    (my/task--goto-task-root)
    (org-entry-put nil property value)))

;;;###autoload
(defun my/task-delete-property (property)
  "Delete managed task PROPERTY from the current task heading."
  (interactive (list (my/task--read-managed-property "Delete property: ")))
  (save-excursion
    (my/task--goto-task-root)
    (org-entry-delete nil property)))

;;;###autoload
(defun my/task-add-worktree-to-gptel-allowed-roots ()
  "Append this task's worktree to the current gptel topic allowed roots."
  (interactive)
  (unless (fboundp 'my/gptel-add-session-allowed-roots)
    (user-error "my-gptel-org-workflow is not loaded"))
  (save-excursion
    (my/task--goto-task-root)
    (let* ((derived (my/task--derived-property-values))
           (worktree (or (my/task--get-property "WORKTREE" t)
                         (cdr (assoc "WORKTREE" derived)))))
      (unless worktree
        (user-error "No WORKTREE property or derived worktree for current task"))
      (my/gptel-add-session-allowed-roots (list worktree)))))

;;;###autoload
(defun my/task-open ()
  "Open the current task's worktree, workspace, and Magit status."
  (interactive)
  (save-excursion
    (my/task--goto-task-root)
    (my/task--open-with-metadata (my/task--collect-metadata))))

;;;###autoload
(defun my/task-init ()
  "Prepare the current task locally without creating a GitHub issue."
  (interactive)
  (save-excursion
    (my/task--goto-task-root)
    (let ((metadata (my/task--collect-metadata)))
      (my/task--write-derived-properties metadata)
      (my/task--open-with-metadata metadata))))

;;;###autoload
(defun my/task-link-issue (issue-ref)
  "Link ISSUE-REF to the current task without creating a new issue."
  (interactive (list (my/task--prompt-issue-reference)))
  (save-excursion
    (my/task--goto-task-root)
    (let* ((metadata (my/task--collect-metadata))
           (issue-data (my/task--normalize-issue-reference
                        issue-ref
                        (plist-get metadata :gh_repo))))
      (setq metadata
            (my/task--metadata-with-overrides
             metadata
             :gh_repo (plist-get issue-data :gh_repo)
             :gh_issue (plist-get issue-data :gh_issue)
             :gh_url (plist-get issue-data :gh_url)))
      (setq metadata (my/task--refresh-derived-metadata metadata))
      (my/task--write-derived-properties metadata)
      metadata)))

;;;###autoload
(defun my/task-insert-gh-link (issue)
  "Insert a repo-aware `gh:owner/repo#ISSUE' Org link for the current task."
  (interactive
   (list
    (save-excursion
      (my/task--goto-task-root)
      (read-string "Issue or PR number: "
                   nil nil
                   (or (my/task--get-property "GH_ISSUE" nil) "")))))
  (let ((gh-repo (save-excursion
                   (my/task--goto-task-root)
                   (let ((repo-path (my/task--repo-path)))
                     (or (my/task--resolve-gh-repo repo-path)
                         (user-error "Could not resolve GH_REPO for current task"))))))
    (unless (string-match-p "\\`[0-9]+\\'" (string-trim issue))
      (user-error "Issue or PR number must be numeric: %s" issue))
    (insert (format "[[gh:%s#%s]]" gh-repo (string-trim issue)))))

;;;###autoload
(defun my/task-init-from-branch (branch)
  "Prepare the current task using an existing local BRANCH."
  (interactive
   (list
    (save-excursion
      (my/task--goto-task-root)
      (my/task--prompt-branch (my/task--repo-path)))))
  (save-excursion
    (my/task--goto-task-root)
    (let* ((metadata (my/task--collect-metadata))
           (branch-slug (my/task--branch-slug branch)))
      (unless (my/task--branch-exists-p (plist-get metadata :repo_path) branch)
        (user-error "Branch does not exist in %s: %s"
                    (plist-get metadata :repo_path)
                    branch))
      (setq metadata
            (my/task--metadata-with-overrides
             metadata
             :branch branch
             :branch_explicit branch
             :workspace (or (plist-get metadata :workspace_explicit)
                            (if (plist-get metadata :gh_issue)
                                (my/task--derive-workspace
                                 (plist-get metadata :repo_name)
                                 (plist-get metadata :gh_issue)
                                 (plist-get metadata :slug))
                              (my/task--derive-workspace
                               (plist-get metadata :repo_name)
                               nil
                               branch-slug)))
             :worktree nil))
      (setq metadata
            (plist-put metadata :worktree
                       (or (plist-get metadata :worktree_explicit)
                           (my/task--derive-worktree
                            (plist-get metadata :repo_path)
                            (plist-get metadata :workspace)))))
      (my/task--write-derived-properties metadata)
      (my/task--open-with-metadata metadata))))

;;;###autoload
(defun my/task-create ()
  "Create or reuse a GitHub issue for the current task, then open it."
  (interactive)
  (save-excursion
    (my/task--goto-task-root)
    (let ((metadata (my/task--collect-metadata)))
      (unless (plist-get metadata :gh_issue)
        (let* ((title (my/task--prompt-issue-title))
               (body (my/task--export-notes-to-markdown))
               (body-file (make-temp-file "my-task-workflow-" nil ".md")))
          (unwind-protect
              (progn
                (with-temp-file body-file
                  (insert body))
                (setq metadata
                      (append
                       (my/task--gh-create-issue (plist-get metadata :repo_path)
                                                 title
                                                 body-file)
                       metadata))
                (setq metadata
                      (my/task--refresh-derived-metadata metadata)))
            (delete-file body-file))))
      (my/task--write-derived-properties metadata)))
  (my/task-open))

;;;###autoload
(defun my/task-insert-skeleton (title)
  "Insert a new repo-plan task skeleton with TITLE at point."
  (interactive "sTask title: ")
  (let ((slug (my/task--slugify title))
        (stars (make-string my/task-heading-level ?*))
        (created (my/task--org-timestamp-now)))
    (unless (bolp)
      (insert "\n"))
    (insert (format "%s TODO %s\n" stars title)
            ":PROPERTIES:\n"
            (format ":CREATED: %s\n" created)
            (format ":GPTEL_TOPIC: %s\n" slug)
            ":END:\n\n"
            (make-string (1+ my/task-heading-level) ?*) " Notes\n\n"
            (make-string (1+ my/task-heading-level) ?*) " Acceptance criteria\n\n"
            (make-string (1+ my/task-heading-level) ?*) " Prompt\n\n"
            (make-string (1+ my/task-heading-level) ?*) " Outcome\n\n")))

(defun my/task--repo-plan-files ()
  "Return repo-plan Org files under `org-directory'."
  (let ((repo-plans (expand-file-name "repo-plans" org-directory)))
    (when (file-directory-p repo-plans)
      (directory-files repo-plans t "\\.org\\'"))))

(defun my/task--clean-heading-title (title)
  "Return TITLE without trailing Org tags."
  (string-trim
   (replace-regexp-in-string
    "[ \t]+:\\(?:[[:alnum:]_@#%]+:\\)+[ \t]*\\'" "" title)))

(defun my/task--property-in-region (beg end property)
  "Return PROPERTY value between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((case-fold-search nil)
          (pattern (format "^[ \t]*:%s:[ \t]*\\(.*\\)$"
                           (regexp-quote property)))
          (value nil))
      (while (and (not value) (re-search-forward pattern end t))
        (let ((trimmed (string-trim (match-string-no-properties 1))))
          (unless (string-empty-p trimmed)
            (setq value trimmed))))
      value)))

(defun my/task--active-headings-in-file (file)
  "Return active task rows from repo-plan FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((rows nil))
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ +\\([[:upper:]][[:upper:]_@#%0-9-]*\\)\\(?: +\\(.*\\)\\)?$" nil t)
        (let ((state (match-string-no-properties 1))
              (title (my/task--clean-heading-title
                      (or (match-string-no-properties 2) "")))
              (body-beg (line-end-position))
              (body-end (save-excursion
                          (if (re-search-forward org-heading-regexp nil t)
                              (line-beginning-position)
                            (point-max)))))
          (when (member state my/task-report-states)
            (push (list :file file
                        :line (line-number-at-pos)
                        :state state
                        :title title
                        :issue (my/task--property-in-region body-beg body-end "GH_ISSUE")
                        :pr (my/task--property-in-region body-beg body-end "PR")
                        :branch (my/task--property-in-region body-beg body-end "BRANCH")
                        :next (my/task--property-in-region body-beg body-end "NEXT_ACTION"))
                  rows))))
      (nreverse rows))))

;;;###autoload
(defun my/task-report-active-repo-tasks ()
  "Show active repo-plan tasks grouped by repo file."
  (interactive)
  (let ((rows (mapcan #'my/task--active-headings-in-file
                      (my/task--repo-plan-files))))
    (with-current-buffer (get-buffer-create "*Org Repo Tasks*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Active repo tasks\n\n")
        (if rows
            (dolist (row rows)
              (insert (format "%s:%s  %-6s  %s\n"
                              (file-relative-name (plist-get row :file) org-directory)
                              (plist-get row :line)
                              (plist-get row :state)
                              (plist-get row :title)))
              (when-let ((issue (plist-get row :issue)))
                (insert (format "  issue: %s\n" issue)))
              (when-let ((pr (plist-get row :pr)))
                (insert (format "  pr: %s\n" pr)))
              (when-let ((branch (plist-get row :branch)))
                (insert (format "  branch: %s\n" branch)))
              (when-let ((next (plist-get row :next)))
                (insert (format "  next: %s\n" next)))
              (insert "\n"))
          (insert "No active repo tasks found.\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer "*Org Repo Tasks*")))

(provide 'my-task-workflow)

;;; my-task-workflow.el ends here
