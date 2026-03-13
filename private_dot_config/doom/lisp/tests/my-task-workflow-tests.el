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

(ert-deftest my/task-init-writes-derived-properties-without-issue ()
  (my/task-test--with-temp-dir repo
    (let* ((repo-path (file-name-as-directory repo))
           (repo-name (file-name-nondirectory (directory-file-name repo-path))))
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
                   (lambda (_metadata) nil)))
          (my/task-init)
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "REPO_PATH" nil) repo-path))
          (should (equal (my/task--get-property "WORKSPACE" nil)
                         (format "%s-task-basis-operator-phase-1" repo-name)))
          (should (equal (my/task--get-property "BRANCH" nil)
                         "basis-operator-phase-1"))
          (should (equal (my/task--get-property "WORKTREE" nil)
                         (expand-file-name
                          (format "%s-task-basis-operator-phase-1" repo-name)
                          (expand-file-name
                           (format "%s-wt" repo-name)
                           (file-name-directory (directory-file-name repo-path)))))))))))

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

(ert-deftest my/task-init-from-branch-writes-branch-based-properties ()
  (my/task-test--with-temp-dir repo
    (let* ((repo-path (file-name-as-directory repo))
           (repo-name (file-name-nondirectory (directory-file-name repo-path))))
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
                   (lambda (_metadata) nil)))
          (my/task-init-from-branch "feature/takeover")
          (my/task--goto-task-root)
          (should (equal (my/task--get-property "BRANCH" nil) "feature/takeover"))
          (should (equal (my/task--get-property "WORKSPACE" nil)
                         (format "%s-task-feature-takeover" repo-name)))
          (should (equal (my/task--get-property "WORKTREE" nil)
                         (expand-file-name
                          (format "%s-task-feature-takeover" repo-name)
                          (expand-file-name
                           (format "%s-wt" repo-name)
                           (file-name-directory (directory-file-name repo-path)))))))))))

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

(provide 'my-task-workflow-tests)

;;; my-task-workflow-tests.el ends here
