;;; my-task-workflow-tests.el --- Tests for task workflow -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(load-file (expand-file-name "../my-task-workflow.el"
                             (file-name-directory load-file-name)))

(defmacro my/task-test--with-org-buffer (content &rest body)
  "Create a temporary Org buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro my/task-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory for BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "my-task-workflow-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun my/task-test--heading (level title)
  "Return an Org heading string for LEVEL and TITLE."
  (format "%s %s\n" (make-string level ?*) title))

(ert-deftest my/task-root-point-resolves-from-task-heading ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task"))
    (re-search-forward "^\\*+ Task$")
    (should (= (my/task--task-root-point) (line-beginning-position)))))

(ert-deftest my/task-root-point-resolves-from-descendants ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task")
              (my/task-test--heading (1+ my/task-heading-level) "Notes")
              "Some notes.\n"
              (my/task-test--heading (1+ my/task-heading-level) "AI response")
              (my/task-test--heading (+ my/task-heading-level 2) "Nested"))
    (let ((task-point (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "^\\*+ Task$")
                        (line-beginning-position))))
      (re-search-forward "^\\*+ AI response$")
      (should (= (my/task--task-root-point) task-point))
      (re-search-forward "^\\*+ Nested$")
      (should (= (my/task--task-root-point) task-point)))))

(ert-deftest my/task-root-point-errors-without-task-heading ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading (1+ my/task-heading-level) "Child"))
    (goto-char (point-max))
    (should-error (my/task--task-root-point) :type 'user-error)))

(ert-deftest my/task-find-notes-selects-direct-child-only ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task")
              (my/task-test--heading (1+ my/task-heading-level) "AI response")
              (my/task-test--heading (+ my/task-heading-level 2) "Notes")
              (my/task-test--heading (1+ my/task-heading-level) "Notes"))
    (goto-char (point-max))
    (let ((marker (my/task--notes-subtree-marker)))
      (unwind-protect
          (progn
            (goto-char marker)
            (should (string-equal (my/task--heading-title) "Notes"))
            (should (= (org-current-level) (1+ my/task-heading-level))))
        (set-marker marker nil)))))

(ert-deftest my/task-derive-names-from-issue ()
  (let* ((repo-path "/tmp/openghg_inversions/")
         (repo-name (my/task--repo-name repo-path))
         (workspace (my/task--derive-workspace repo-name "417" "basis-operator-phase-1"))
         (worktree-root (my/task--worktree-root repo-path))
         (worktree (my/task--derive-worktree repo-path workspace)))
    (should (equal workspace "openghg_inversions-iss-417"))
    (should (equal worktree-root "/tmp/openghg_inversions-wt"))
    (should (equal worktree "/tmp/openghg_inversions-wt/openghg_inversions-iss-417"))
    (should (equal (my/task--derive-branch "417" "basis-operator-phase-1")
                   "issue-417-basis-operator-phase-1"))))

(ert-deftest my/task-derive-names-without-issue ()
  (let* ((repo-path "/tmp/openghg_inversions/")
         (repo-name (my/task--repo-name repo-path))
         (workspace (my/task--derive-workspace repo-name nil "basis-operator-phase-1")))
    (should (equal workspace "openghg_inversions-task-basis-operator-phase-1"))
    (should (equal (my/task--derive-worktree repo-path workspace)
                   "/tmp/openghg_inversions-wt/openghg_inversions-task-basis-operator-phase-1"))
    (should (equal (my/task--derive-branch nil "basis-operator-phase-1")
                   "basis-operator-phase-1"))))

(ert-deftest my/task-parse-gh-output ()
  (let ((parsed (my/task--parse-gh-issue-output "https://github.com/org/repo/issues/417\n")))
    (should (equal (plist-get parsed :gh_url) "https://github.com/org/repo/issues/417"))
    (should (equal (plist-get parsed :gh_issue) "417"))))

(ert-deftest my/task-parse-gh-output-errors-on-malformed-text ()
  (should-error (my/task--parse-gh-issue-output "created issue 417") :type 'user-error))

(ert-deftest my/task-collect-metadata-derives-missing-values ()
  (my/task-test--with-temp-dir repo
    (let* ((repo-path (file-name-as-directory repo))
           (repo-name (file-name-nondirectory (directory-file-name repo-path)))
           (expected-workspace (format "%s-iss-417" repo-name))
           (expected-worktree
            (expand-file-name
             expected-workspace
             (expand-file-name
              (format "%s-wt" repo-name)
              (file-name-directory (directory-file-name repo-path))))))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  ":PROPERTIES:\n:GH_ISSUE: 417\n:END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path)))
          (let ((metadata (my/task--collect-metadata)))
            (should (equal (plist-get metadata :repo_path) repo-path))
            (should (equal (plist-get metadata :workspace) expected-workspace))
            (should (equal (plist-get metadata :branch)
                           "issue-417-basis-operator-phase-1"))
            (should (equal (plist-get metadata :worktree) expected-worktree))))))))

(ert-deftest my/task-init-writes-metadata-only-without-issue ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  (my/task-test--heading (1+ my/task-heading-level) "Notes")
                  "Some notes.\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--open-with-metadata)
                   (lambda (_metadata) (setq opened t))))
          (my/task-init)
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "REPO_PATH" nil) repo-path))
          (should (equal (my/task--get-property "GPTEL_TOPIC" nil)
                         "basis-operator-phase-1"))
          (should (my/task--get-property "CREATED" nil))
          (should (equal (my/task--get-property "BRANCH" nil)
                         "basis-operator-phase-1"))
          (should-not (my/task--get-property "WORKSPACE" nil))
          (should-not (my/task--get-property "WORKTREE" nil))
          (should-not opened))))))

(ert-deftest my/task-link-issue-writes-gh-properties ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo)))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  ":PROPERTIES:\n:WORKSPACE: local-ws\n:BRANCH: local-branch\n:WORKTREE: /tmp/local-ws\n:END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path)))
          (my/task-link-issue "https://github.com/org/repo/issues/417")
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "GH_ISSUE" nil) "417"))
          (should (equal (my/task--get-property "GH_URL" nil)
                         "https://github.com/org/repo/issues/417"))
          (should (equal (my/task--get-property "WORKSPACE" nil) "local-ws"))
          (should (equal (my/task--get-property "BRANCH" nil) "local-branch"))
          (should (equal (my/task--get-property "WORKTREE" nil) "/tmp/local-ws")))))))

(ert-deftest my/task-create-writes-issue-metadata-without-opening-worktree ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  (my/task-test--heading (1+ my/task-heading-level) "Notes")
                  "Some notes.\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--prompt-issue-title)
                   (lambda () "Basis operator phase 1"))
                  ((symbol-function 'my/task--gh-create-issue)
                   (lambda (_repo _title _body-file)
                     '(:gh_repo "org/repo"
                       :gh_issue "417"
                       :gh_url "https://github.com/org/repo/issues/417")))
                  ((symbol-function 'my/task--open-with-metadata)
                   (lambda (_metadata) (setq opened t))))
          (my/task-create)
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "GH_REPO" nil) "org/repo"))
          (should (equal (my/task--get-property "GH_ISSUE" nil) "417"))
          (should (equal (my/task--get-property "BRANCH" nil)
                         "issue-417-basis-operator-phase-1"))
          (should-not (my/task--get-property "WORKSPACE" nil))
          (should-not (my/task--get-property "WORKTREE" nil))
          (should-not opened))))))

(ert-deftest my/task-init-from-branch-writes-branch-metadata-only ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  (my/task-test--heading (1+ my/task-heading-level) "Notes")
                  "Some notes.\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--branch-exists-p)
                   (lambda (_repo branch)
                     (string-equal branch "feature/takeover")))
                  ((symbol-function 'my/task--open-with-metadata)
                   (lambda (_metadata) (setq opened t))))
          (my/task-init-from-branch "feature/takeover")
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "BRANCH" nil) "feature/takeover"))
          (should-not (my/task--get-property "WORKSPACE" nil))
          (should-not (my/task--get-property "WORKTREE" nil))
          (should-not opened))))))

(ert-deftest my/task-ensure-worktree-creates-worktree-root-and-branch ()
  (let ((created-dir nil)
        (commands nil))
    (cl-letf (((symbol-function 'make-directory)
               (lambda (dir &optional _parents)
                 (setq created-dir dir)))
              ((symbol-function 'file-directory-p)
               (lambda (_path) nil))
              ((symbol-function 'my/task--branch-exists-p)
               (lambda (_repo _branch) nil))
              ((symbol-function 'my/task--assert-base-ready)
               (lambda (_repo _base) nil))
              ((symbol-function 'my/task--call-process-string)
               (lambda (program &rest args)
                 (push (cons program args) commands)
                 "")))
      (my/task--ensure-worktree "/tmp/openghg_inversions/"
                                "issue-417-basis-operator-phase-1"
                                "/tmp/openghg_inversions-wt/openghg_inversions-iss-417")
      (should (equal created-dir "/tmp/openghg_inversions-wt"))
      (should (equal (car commands)
                     '("git" "worktree" "add" "-b" "issue-417-basis-operator-phase-1"
                      "/tmp/openghg_inversions-wt/openghg_inversions-iss-417" "HEAD"))))))

(ert-deftest my/task-ensure-worktree-checks-base-before-new-branch ()
  (let ((checked nil))
    (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) nil))
              ((symbol-function 'file-directory-p) (lambda (_path) nil))
              ((symbol-function 'my/task--branch-exists-p) (lambda (&rest _) nil))
              ((symbol-function 'my/task--assert-base-ready)
               (lambda (repo base)
                 (setq checked (list repo base))))
              ((symbol-function 'my/task--call-process-string)
               (lambda (&rest _) "")))
      (my/task--ensure-worktree "/tmp/repo/" "task-branch" "/tmp/repo-wt/task" "devel")
      (should (equal checked '("/tmp/repo/" "devel"))))))

(ert-deftest my/task-assert-base-ready-rejects-wrong-current-branch ()
  (cl-letf (((symbol-function 'my/task--current-branch)
             (lambda (_repo) "feature/in-progress")))
    (should-error (my/task--assert-base-ready "/tmp/repo/" "main")
                  :type 'user-error)))

(ert-deftest my/task-parse-worktree-list-captures-branches-and-detached ()
  (let ((entries (my/task--parse-worktree-list
                  (concat "worktree /tmp/repo\n"
                          "HEAD abc\n"
                          "branch refs/heads/main\n"
                          "\n"
                          "worktree /tmp/repo-wt/task\n"
                          "HEAD def\n"
                          "branch refs/heads/codex/feature\n"
                          "\n"
                          "worktree /tmp/repo-wt/detached\n"
                          "HEAD 123\n"
                          "detached\n"))))
    (should (= (length entries) 3))
    (should (equal (plist-get (car entries) :path) "/tmp/repo"))
    (should (equal (plist-get (car entries) :branch) "main"))
    (should (equal (plist-get (cadr entries) :branch) "codex/feature"))
    (should (plist-get (caddr entries) :detached))
    (should-not (plist-get (caddr entries) :branch))))

(ert-deftest my/task-adopt-worktree-writes-worktree-and-branch-only ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo)))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Task"))
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--worktrees)
                   (lambda (_repo)
                     (list (list :path "/tmp/codex/repo"
                                 :branch "codex/task")))))
          (my/task-adopt-worktree "/tmp/codex/repo")
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "WORKTREE" nil)
                         "/tmp/codex/repo"))
          (should (equal (my/task--get-property "BRANCH" nil)
                         "codex/task"))
          (should-not (my/task--get-property "WORKSPACE" nil)))))))

(ert-deftest my/task-open-uses-explicit-worktree-property ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil)
          (workspace nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Task")
                  ":PROPERTIES:\n"
                  ":BRANCH: codex/task\n"
                  ":WORKTREE: /tmp/codex/repo\n"
                  ":END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--ensure-worktree)
                   (lambda (_repo _branch worktree &optional _base)
                     (setq opened worktree)))
                  ((symbol-function 'my/task--switch-workspace)
                   (lambda (name) (setq workspace name)))
                  ((symbol-function 'my/task--open-worktree) (lambda (&rest _) nil))
                  ((symbol-function 'my/task--magit-status) (lambda (&rest _) nil)))
          (my/task-open)
          (should (equal opened "/tmp/codex/repo"))
          (should (equal workspace
                         (format "%s-task-codex-task"
                                 (file-name-nondirectory
                                  (directory-file-name repo-path))))))))))

(ert-deftest my/task-open-adopts-unique-worktree-for-branch ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Task")
                  ":PROPERTIES:\n:BRANCH: codex/task\n:END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--worktrees)
                   (lambda (_repo)
                     (list (list :path repo-path :branch "main")
                           (list :path "/tmp/codex/repo" :branch "codex/task"))))
                  ((symbol-function 'my/task--ensure-worktree)
                   (lambda (_repo _branch worktree &optional _base)
                     (setq opened worktree)))
                  ((symbol-function 'my/task--switch-workspace) (lambda (&rest _) nil))
                  ((symbol-function 'my/task--open-worktree) (lambda (&rest _) nil))
                  ((symbol-function 'my/task--magit-status) (lambda (&rest _) nil)))
          (my/task-open)
          (my/task--goto-task-root)
          (should (equal opened "/tmp/codex/repo"))
          (should (equal (my/task--get-property "WORKTREE" nil)
                         "/tmp/codex/repo")))))))

(ert-deftest my/task-open-codex-uses-primary-repo-not-worktree ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo))
          (opened nil)
          (prompt nil))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Task")
                  ":PROPERTIES:\n"
                  ":BRANCH: codex/task\n"
                  ":WORKTREE: /tmp/codex/repo\n"
                  ":END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--primary-worktree-path)
                   (lambda (_repo) repo-path))
                  ((symbol-function 'my/task--open-in-codex)
                   (lambda (repo) (setq opened repo)))
                  ((symbol-function 'my/task--show-codex-handoff)
                   (lambda (text) (setq prompt text))))
          (my/task-open-codex)
          (should (equal opened repo-path))
          (should (string-match-p "Suggested branch: codex/task" prompt))
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "WORKTREE" nil)
                         "/tmp/codex/repo")))))))

(ert-deftest my/task-active-headings-report-parses-properties ()
  (let ((file nil))
    (unwind-protect
        (progn
          (setq file (make-temp-file "my-task-report-" nil ".org"))
          (with-temp-file file
            (insert "#+title: Repo\n"
                    "** TODO Active task :ATTACH:\n"
                    ":PROPERTIES:\n"
                    ":GH_ISSUE: 12\n"
                    ":PR: 34\n"
                    ":BRANCH: issue-12-active-task\n"
                    ":NEXT_ACTION: run tests\n"
                    ":END:\n"
                    "** DONE Finished task\n"))
          (let ((rows (my/task--active-headings-in-file file)))
            (should (= (length rows) 1))
            (should (equal (plist-get (car rows) :state) "TODO"))
            (should (equal (plist-get (car rows) :title) "Active task"))
            (should (equal (plist-get (car rows) :issue) "12"))
            (should (equal (plist-get (car rows) :pr) "34"))
            (should (equal (plist-get (car rows) :branch) "issue-12-active-task"))
            (should (equal (plist-get (car rows) :next) "run tests"))))
      (when (and file (file-exists-p file))
        (delete-file file)))))

(ert-deftest my/task-ensure-worktree-uses-existing-branch ()
  (let ((commands nil))
    (cl-letf (((symbol-function 'make-directory) (lambda (&rest _) nil))
              ((symbol-function 'file-directory-p) (lambda (_path) nil))
              ((symbol-function 'my/task--branch-exists-p) (lambda (&rest _) t))
              ((symbol-function 'my/task--call-process-string)
               (lambda (program &rest args)
                 (push (cons program args) commands)
                 "")))
      (my/task--ensure-worktree "/tmp/openghg_inversions/"
                                "issue-417-basis-operator-phase-1"
                                "/tmp/openghg_inversions-wt/openghg_inversions-iss-417")
      (should (equal (car commands)
                     '("git" "worktree" "add"
                       "/tmp/openghg_inversions-wt/openghg_inversions-iss-417"
                       "issue-417-basis-operator-phase-1"))))))

(ert-deftest my/task-normalize-issue-reference-number-uses-gh-repo ()
  (let ((parsed (my/task--normalize-issue-reference "417" "org/repo")))
    (should (equal (plist-get parsed :gh_repo) "org/repo"))
    (should (equal (plist-get parsed :gh_issue) "417"))
    (should (equal (plist-get parsed :gh_url)
                   "https://github.com/org/repo/issues/417")))
  (should-error (my/task--normalize-issue-reference "417") :type 'user-error))

(ert-deftest my/task-normalize-issue-reference-url-captures-repo ()
  (let ((parsed (my/task--normalize-issue-reference
                 "https://github.com/org/repo/issues/417")))
    (should (equal (plist-get parsed :gh_repo) "org/repo"))
    (should (equal (plist-get parsed :gh_issue) "417"))
    (should (equal (plist-get parsed :gh_url)
                   "https://github.com/org/repo/issues/417"))))

(ert-deftest my/task-resolve-gh-repo-prefers-property-then-url ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task")
              ":PROPERTIES:\n:GH_REPO: direct/repo\n:GH_URL: https://github.com/url/repo/issues/9\n:END:\n")
    (goto-char (point-max))
    (my/task--goto-task-root)
    (should (equal (my/task--resolve-gh-repo nil) "direct/repo")))
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task")
              ":PROPERTIES:\n:GH_URL: https://github.com/url/repo/issues/9\n:END:\n")
    (goto-char (point-max))
    (my/task--goto-task-root)
    (cl-letf (((symbol-function 'my/task--gh-repo-from-gh) (lambda (_repo) nil))
              ((symbol-function 'my/task--gh-repo-from-remote) (lambda (_repo) nil)))
      (should (equal (my/task--resolve-gh-repo nil) "url/repo")))))

(ert-deftest my/task-resolve-gh-repo-falls-back-to-gh-then-remote ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task"))
    (goto-char (point-max))
    (my/task--goto-task-root)
    (cl-letf (((symbol-function 'my/task--gh-repo-from-gh)
               (lambda (_repo) "gh/repo"))
              ((symbol-function 'my/task--gh-repo-from-remote)
               (lambda (_repo) "remote/repo")))
      (should (equal (my/task--resolve-gh-repo "/tmp/repo/") "gh/repo")))
    (cl-letf (((symbol-function 'my/task--gh-repo-from-gh)
               (lambda (_repo) nil))
              ((symbol-function 'my/task--gh-repo-from-remote)
               (lambda (_repo) "remote/repo")))
      (should (equal (my/task--resolve-gh-repo "/tmp/repo/") "remote/repo")))))

(ert-deftest my/task-link-issue-number-writes-repo-issue-and-url ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo)))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  ":PROPERTIES:\n:GH_REPO: org/repo\n:WORKSPACE: local-ws\n:BRANCH: local-branch\n:WORKTREE: /tmp/local-ws\n:END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path)))
          (my/task-link-issue "417")
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "GH_REPO" nil) "org/repo"))
          (should (equal (my/task--get-property "GH_ISSUE" nil) "417"))
          (should (equal (my/task--get-property "GH_URL" nil)
                         "https://github.com/org/repo/issues/417"))
          (should (equal (my/task--get-property "WORKSPACE" nil) "local-ws")))))))

(ert-deftest my/task-property-set-delete-operates-on-task-root ()
  (my/task-test--with-org-buffer
      (concat (my/task-test--heading 1 "Repo")
              (my/task-test--heading my/task-heading-level "Task")
              (my/task-test--heading (1+ my/task-heading-level) "Notes")
              "Body\n")
    (goto-char (point-max))
    (my/task-set-property "NEXT_ACTION" "run tests")
    (my/task--goto-task-root)
    (should (equal (my/task--get-property "NEXT_ACTION" nil) "run tests"))
    (goto-char (point-max))
    (my/task-delete-property "NEXT_ACTION")
    (my/task--goto-task-root)
    (should-not (my/task--get-property "NEXT_ACTION" nil))))

(ert-deftest my/task-property-snapshot-includes-derived-values ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo)))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Basis operator phase 1")
                  ":PROPERTIES:\n:GH_ISSUE: 417\n:END:\n")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path))
                  ((symbol-function 'my/task--resolve-gh-repo)
                   (lambda (&optional _repo) "org/repo")))
          (let* ((rows (my/task--property-snapshot))
                 (gh-url (cl-find "GH_URL" rows
                                  :key (lambda (row) (plist-get row :property))
                                  :test #'equal))
                 (worktree (cl-find "WORKTREE" rows
                                    :key (lambda (row) (plist-get row :property))
                                    :test #'equal)))
            (should (equal (plist-get gh-url :derived)
                           "https://github.com/org/repo/issues/417"))
            (should (string-match-p "iss-417"
                                    (plist-get worktree :derived)))))))))

(ert-deftest my/task-insert-gh-link-inserts-at-original-point ()
  (my/task-test--with-temp-dir repo
    (let ((repo-path (file-name-as-directory repo)))
      (my/task-test--with-org-buffer
          (concat "#+PROPERTY: REPO_PATH " repo-path "\n"
                  (my/task-test--heading 1 "Repo")
                  (my/task-test--heading my/task-heading-level "Task")
                  ":PROPERTIES:\n:GH_REPO: org/repo\n:END:\n\nInsert here: ")
        (goto-char (point-max))
        (cl-letf (((symbol-function 'my/task--git-root)
                   (lambda (_path) repo-path)))
          (my/task-insert-gh-link "123")
          (should (string-suffix-p "Insert here: [[gh:org/repo#123]]"
                                   (string-trim-right (buffer-string)))))))))

(ert-deftest my/task-add-worktree-to-gptel-allowed-roots-uses-task-worktree ()
  (let (captured)
    (my/task-test--with-org-buffer
        (concat (my/task-test--heading 1 "Repo")
                (my/task-test--heading my/task-heading-level "Task")
                ":PROPERTIES:\n:WORKTREE: /tmp/repo-wt/task\n:END:\n")
      (goto-char (point-max))
      (cl-letf (((symbol-function 'my/gptel-add-session-allowed-roots)
                 (lambda (roots) (setq captured roots))))
        (my/task-add-worktree-to-gptel-allowed-roots)
        (should (equal captured '("/tmp/repo-wt/task")))))))

(provide 'my-task-workflow-tests)

;;; my-task-workflow-tests.el ends here
