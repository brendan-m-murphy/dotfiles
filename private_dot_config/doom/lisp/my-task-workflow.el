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

(defgroup my/task-workflow nil
  "Task-centric Org workflow helpers."
  :group 'tools)

(defcustom my/task-heading-level 2
  "Org heading level used as the task root."
  :type 'integer
  :group 'my/task-workflow)

(defconst my/task-properties
  '("REPO_PATH" "GH_ISSUE" "GH_URL" "BRANCH" "WORKTREE" "WORKSPACE")
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

(defun my/task--heading-slug ()
  "Return a slug derived from the current task heading."
  (let* ((title (downcase (my/task--heading-title)))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" title)))
    (string-trim slug "-" "-")))

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

(defun my/task--normalize-issue-reference (issue-ref)
  "Normalize ISSUE-REF into a plist with :gh_issue and optional :gh_url."
  (let ((trimmed (string-trim issue-ref)))
    (cond
     ((string-match "\\`[0-9]+\\'" trimmed)
      (list :gh_issue trimmed))
     ((string-match "/issues/\\([0-9]+\\)/*\\'" trimmed)
      (list :gh_issue (match-string 1 trimmed)
            :gh_url trimmed))
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

(defun my/task--parse-gh-issue-output (output)
  "Parse gh issue creation OUTPUT into a plist."
  (let ((trimmed (string-trim output)))
    (unless (string-match "/issues/\\([0-9]+\\)/*\\'" trimmed)
      (user-error "Could not parse gh issue output: %s" output))
    (list :gh_url trimmed
          :gh_issue (match-string 1 trimmed))))

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

(defun my/task--ensure-worktree (repo-path branch worktree)
  "Ensure WORKTREE exists for BRANCH in REPO-PATH."
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
         (slug (my/task--heading-slug))
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
          :repo_name repo-name
          :gh_issue issue
          :gh_url (my/task--get-property "GH_URL" nil)
          :slug slug
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
        (workspace (plist-get metadata :workspace)))
    (my/task--ensure-worktree repo-path branch worktree)
    (my/task--write-derived-properties metadata)
    (my/task--switch-workspace workspace)
    (my/task--open-worktree worktree)
    (my/task--magit-status worktree)))

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
           (issue-data (my/task--normalize-issue-reference issue-ref)))
      (setq metadata
            (my/task--metadata-with-overrides
             metadata
             :gh_issue (plist-get issue-data :gh_issue)
             :gh_url (or (plist-get issue-data :gh_url)
                         (plist-get metadata :gh_url))))
      (setq metadata (my/task--refresh-derived-metadata metadata))
      (my/task--write-derived-properties metadata)
      metadata)))

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

(provide 'my-task-workflow)

;;; my-task-workflow.el ends here
