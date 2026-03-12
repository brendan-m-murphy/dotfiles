;;; my-gptel-tools-edit-tests.el --- Tests for gptel editing tools -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)

(load-file (expand-file-name "../my-gptel-tools.el"
                             (file-name-directory load-file-name)))

(defmacro my/gptel-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory for BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "my-gptel-tools-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defmacro my/gptel-test--with-buffer (roots &rest body)
  "Run BODY in a temporary buffer with ROOTS as the allowed roots."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((allowed-roots ,roots))
       (cl-letf (((symbol-function 'my/gptel--allowed-roots)
                  (lambda () allowed-roots))
                 ((symbol-function 'my/gptel--project-root)
                  (lambda () nil)))
         ,@body))))

(defun my/gptel-test--dir (path)
  "Return PATH as a canonical directory name."
  (file-name-as-directory (file-truename path)))

(ert-deftest my/gptel-write-root-required-for-mutations ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "file.txt" root)))
      (my/gptel-test--with-buffer (list (my/gptel-test--dir root))
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-write-file :path path :content "hello"))
         :type 'user-error)
        (should-not my/gptel-edit-session-started)
        (should-not (file-exists-p path))))))

(ert-deftest my/gptel-write-root-manual-allows-in-scope-write ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "notes.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
          (my/gptel-tool-write-file :path path :content "hello"))
        (should (equal (my/gptel--read-file-string path) "hello"))
        (should my/gptel-edit-session-started)))))

(ert-deftest my/gptel-write-root-manual-rejects-out-of-scope-write ()
  (my/gptel-test--with-temp-dir root-a
    (my/gptel-test--with-temp-dir root-b
      (let ((path (expand-file-name "other.txt" root-b))
            (root-a-dir (my/gptel-test--dir root-a))
            (root-b-dir (my/gptel-test--dir root-b)))
        (my/gptel-test--with-buffer (list root-a-dir root-b-dir)
          (my/gptel--set-write-root-internal root-a-dir 'manual)
          (should-error
           (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
             (my/gptel-tool-write-file :path path :content "nope"))
           :type 'user-error)
          (should-not (file-exists-p path))
          (should-not my/gptel-edit-session-started))))))

(ert-deftest my/gptel-first-write-requires-clean-worktree ()
  (my/gptel-test--with-temp-dir repo
    (let ((path (expand-file-name "file.txt" repo))
          (repo-dir (my/gptel-test--dir repo)))
      (my/gptel-test--with-buffer (list repo-dir)
        (my/gptel--set-write-root-internal repo-dir 'manual)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) repo-dir))
                    ((symbol-function 'my/gptel--git-worktree-clean-p) (lambda (_root) nil)))
           (my/gptel-tool-write-file :path path :content "blocked"))
         :type 'user-error)
        (should-not my/gptel-edit-session-started)
        (should-not (file-exists-p path))))))

(ert-deftest my/gptel-subsequent-write-same-session-allowed-when-dirty ()
  (my/gptel-test--with-temp-dir repo
    (let ((path-a (expand-file-name "a.txt" repo))
          (path-b (expand-file-name "b.txt" repo))
          (repo-dir (my/gptel-test--dir repo))
          (checks 0))
      (my/gptel-test--with-buffer (list repo-dir)
        (my/gptel--set-write-root-internal repo-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) repo-dir))
                   ((symbol-function 'my/gptel--git-worktree-clean-p)
                    (lambda (_root)
                      (cl-incf checks)
                      t)))
          (my/gptel-tool-write-file :path path-a :content "one")
          (my/gptel-tool-write-file :path path-b :content "two"))
        (should (= checks 1))
        (should (equal (my/gptel--read-file-string path-b) "two"))))))

(ert-deftest my/gptel-write-session-locks-to-first-repo ()
  (my/gptel-test--with-temp-dir workspace
    (let* ((repo-a (expand-file-name "repo-a" workspace))
           (repo-b (expand-file-name "repo-b" workspace))
           (file-a (expand-file-name "a.txt" repo-a))
           (file-b (expand-file-name "b.txt" repo-b))
           (workspace-dir (my/gptel-test--dir workspace))
           (repo-a-dir (my/gptel-test--dir repo-a))
           (repo-b-dir (my/gptel-test--dir repo-b)))
      (make-directory repo-a t)
      (make-directory repo-b t)
      (my/gptel-test--with-buffer (list workspace-dir)
        (my/gptel--set-write-root-internal workspace-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root)
                   (lambda (path)
                     (cond
                      ((string-prefix-p repo-a-dir (my/gptel--canonical-path path)) repo-a-dir)
                      ((string-prefix-p repo-b-dir (my/gptel--canonical-path path)) repo-b-dir)
                      (t nil))))
                  ((symbol-function 'my/gptel--git-worktree-clean-p) (lambda (_root) t)))
          (my/gptel-tool-write-file :path file-a :content "repo-a")
          (should (string-equal my/gptel-write-root repo-a-dir))
          (should-error
           (my/gptel-tool-write-file :path file-b :content "repo-b")
           :type 'user-error))))))

(ert-deftest my/gptel-org-property-write-root-resolves-outside-chat-file-repo ()
  (my/gptel-test--with-temp-dir chat-root
    (my/gptel-test--with-temp-dir repo
      (let ((repo-dir (my/gptel-test--dir repo))
            (chat-file (expand-file-name "chat.org" chat-root))
            (target (expand-file-name "src/file.txt" repo)))
        (my/gptel-test--with-buffer (list (my/gptel-test--dir chat-root) repo-dir)
          (org-mode)
          (setq buffer-file-name chat-file)
          (insert (format "* Topic\n:PROPERTIES:\n:GPTEL_WRITE_ROOT: %s\n:END:\n\n" repo-dir))
          (goto-char (point-max))
          (cl-letf (((symbol-function 'my/gptel--git-root)
                     (lambda (path)
                       (when (string-prefix-p repo-dir (my/gptel--canonical-path path))
                         repo-dir)))
                    ((symbol-function 'my/gptel--git-worktree-clean-p) (lambda (_root) t)))
            (my/gptel-tool-write-file :path target :content "ok"))
          (should (string-equal my/gptel-write-root repo-dir))
          (should (equal (my/gptel--read-file-string target) "ok")))))))

(ert-deftest my/gptel-chat-file-location-does-not-drive-write-root ()
  (my/gptel-test--with-temp-dir chat-root
    (my/gptel-test--with-temp-dir target-root
      (let ((chat-file (expand-file-name "chat.org" chat-root))
            (target-file (expand-file-name "src/file.txt" target-root))
            (target-dir (my/gptel-test--dir target-root)))
        (my/gptel-test--with-buffer (list (my/gptel-test--dir chat-root) target-dir)
          (org-mode)
          (setq buffer-file-name chat-file)
          (cl-letf (((symbol-function 'my/gptel--git-root)
                     (lambda (path)
                       (when (string-prefix-p target-dir (my/gptel--canonical-path path))
                         target-dir))))
            (should (string-equal (my/gptel--resolve-write-root target-file)
                                  target-dir))
            (should (string-equal my/gptel-write-root target-dir))))))))

(ert-deftest my/gptel-replace-region-exact-match-succeeds ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "edit.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "alpha beta"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
          (my/gptel-tool-replace-region
           :path path
           :old_text "beta"
           :new_text "gamma"))
        (should (equal (my/gptel--read-file-string path) "alpha gamma"))))))

(ert-deftest my/gptel-replace-region-mismatch-fails-without-change ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "edit.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "alpha beta"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-replace-region
            :path path
            :old_text "missing"
            :new_text "gamma"))
         :type 'user-error)
        (should (equal (my/gptel--read-file-string path) "alpha beta"))))))

(ert-deftest my/gptel-replace-region-multiple-matches-requires-occurrence ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "edit.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "foo foo"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-replace-region
            :path path
            :old_text "foo"
            :new_text "bar"))
         :type 'user-error)
        (should (equal (my/gptel--read-file-string path) "foo foo"))))))

(ert-deftest my/gptel-replace-region-occurrence-replaces-only-requested-match ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "edit.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "foo foo"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
          (my/gptel-tool-replace-region
           :path path
           :old_text "foo"
           :new_text "bar"
           :occurrence 2))
        (should (equal (my/gptel--read-file-string path) "foo bar"))))))

(ert-deftest my/gptel-failed-mutation-does-not-start-session ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "edit.txt" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "alpha"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-replace-region
            :path path
            :old_text "missing"
            :new_text "beta"))
         :type 'user-error)
        (should-not my/gptel-edit-session-started)))))

(ert-deftest my/gptel-non-git-path-write-refused-by-default ()
  (my/gptel-test--with-temp-dir chat-root
    (my/gptel-test--with-temp-dir target-root
      (let ((target-dir (my/gptel-test--dir target-root)))
        (my/gptel-test--with-buffer (list (my/gptel-test--dir chat-root) target-dir)
          (org-mode)
          (insert (format "* Topic\n:PROPERTIES:\n:GPTEL_WRITE_ROOT: %s\n:END:\n\n"
                          target-dir))
          (goto-char (point-max))
          (should-error
           (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
             (my/gptel-tool-write-file
              :path (expand-file-name "file.txt" target-root)
              :content "blocked"))
           :type 'user-error))))))

(ert-deftest my/gptel-binary-file-write-refused ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "blob.bin" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "abc\0def"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-write-file :path path :content "text"))
         :type 'user-error)
        (should-error
         (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
           (my/gptel-tool-replace-region
            :path path
            :old_text "abc"
            :new_text "xyz"))
         :type 'user-error)))))

(ert-deftest my/gptel-delete-file-allows-binary-targets ()
  (my/gptel-test--with-temp-dir root
    (let ((path (expand-file-name "blob.bin" root))
          (root-dir (my/gptel-test--dir root)))
      (with-temp-file path
        (insert "abc\0def"))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (cl-letf (((symbol-function 'my/gptel--git-root) (lambda (_path) nil)))
          (my/gptel-tool-delete-file :path path))
        (should-not (file-exists-p path))))))

(ert-deftest my/gptel-remote-path-write-refused ()
  (my/gptel-test--with-temp-dir root
    (let ((root-dir (my/gptel-test--dir root)))
      (my/gptel-test--with-buffer (list root-dir)
        (my/gptel--set-write-root-internal root-dir 'manual)
        (should-error
         (my/gptel-tool-write-file :path "/sshx:user@example:/tmp/file.txt" :content "blocked")
         :type 'user-error)))))

(ert-deftest my/gptel-reset-edit-session-clears-state ()
  (my/gptel-test--with-temp-dir root
    (let ((root-dir (my/gptel-test--dir root)))
      (my/gptel-test--with-buffer (list root-dir)
        (setq-local my/gptel-write-root root-dir)
        (setq-local my/gptel-edit-session-started t)
        (my/gptel-reset-edit-session)
        (should-not my/gptel-write-root)
        (should-not my/gptel-edit-session-started)))))

(provide 'my-gptel-tools-edit-tests)
;;; my-gptel-tools-edit-tests.el ends here
