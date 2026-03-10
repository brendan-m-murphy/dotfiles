;;; lisp/tests/macher-custom-tests.el --- Tests for macher workspace resolver -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(unless (featurep 'gptel)
  (defun gptel-send (&rest _args) nil)
  (provide 'gptel))

(load-file (expand-file-name "../macher-custom.el"
                             (file-name-directory load-file-name)))

(defmacro my/macher-test--with-org-buffer (content &rest body)
  "Create a temporary Org buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest my/macher-org-property-override ()
  (let* ((tmp (make-temp-file "macher-org-root" t))
         (expected (expand-file-name tmp)))
    (unwind-protect
        (my/macher-test--with-org-buffer
            (format "* Task\n:PROPERTIES:\n:MACHER_ROOT: %s\n:END:\n" tmp)
          (goto-char (point-max))
          (should (equal (my/macher-root-from-org) expected))
          (should (equal (my/macher-resolve-workspace-root) expected)))
      (delete-directory tmp t))))

(ert-deftest my/macher-repo-detection-from-buffer-reference ()
  (let* ((tmp (make-temp-file "macher-repo" t))
         (repo (expand-file-name "repo" tmp))
         (file (expand-file-name "file.py" repo)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" repo) t)
          (with-temp-file file (insert "print('ok')\n"))
          (with-temp-buffer
            (insert file)
            (setq default-directory tmp)
            (should (equal (my/macher-root-from-buffer-files)
                           (file-name-as-directory repo)))))
      (delete-directory tmp t))))

(ert-deftest my/macher-allowed-roots-fallback ()
  (let* ((tmp (make-temp-file "macher-allowed" t))
         (docs (expand-file-name "Documents" tmp))
         (bufdir (expand-file-name "org/projects" docs))
         (my/gptel-allowed-roots (list docs)))
    (unwind-protect
        (progn
          (make-directory bufdir t)
          (with-temp-buffer
            (setq default-directory (file-name-as-directory bufdir))
            (should (equal (my/macher-resolve-workspace-root)
                           (expand-file-name docs)))))
      (delete-directory tmp t))))

(ert-deftest my/macher-deepest-allowed-root-wins ()
  (let* ((tmp (make-temp-file "macher-deepest" t))
         (docs (expand-file-name "Documents" tmp))
         (work (expand-file-name "work" docs))
         (project (expand-file-name "project" work))
         (my/gptel-allowed-roots (list docs work)))
    (unwind-protect
        (progn
          (make-directory project t)
          (with-temp-buffer
            (setq default-directory (file-name-as-directory project))
            (should (equal (my/macher-root-from-allowed-roots)
                           (expand-file-name work)))))
      (delete-directory tmp t))))

(ert-deftest my/macher-no-match-returns-nil ()
  (let* ((tmp (make-temp-file "macher-none" t))
         (outside (expand-file-name "outside" tmp))
         (my/gptel-allowed-roots (list (expand-file-name "allowed" tmp))))
    (unwind-protect
        (progn
          (make-directory outside t)
          (with-temp-buffer
            (insert "No paths and no properties")
            (setq default-directory (file-name-as-directory outside))
            (should-not (my/macher-resolve-workspace-root))))
      (delete-directory tmp t))))

(provide 'macher-custom-tests)
;;; macher-custom-tests.el ends here
