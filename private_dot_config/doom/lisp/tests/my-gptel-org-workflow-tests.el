;;; my-gptel-org-workflow-tests.el --- Tests for gptel org workflow -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)

(unless (featurep 'gptel)
  (provide 'gptel))

(load-file (expand-file-name "../my-gptel-org-workflow.el"
                             (file-name-directory load-file-name)))

(defmacro my/gptel-test--with-org-buffer (content &rest body)
  "Create a temporary Org buffer with CONTENT and run BODY."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest my/gptel-clamp-response-headings-basic-clamp ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* Title\n** Section\n"
    (my/gptel-clamp-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** Title\n**** Section\n"))))

(ert-deftest my/gptel-clamp-response-headings-already-valid-unchanged ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n**** Title\n***** Section\n"
    (let ((before (buffer-string)))
      (my/gptel-clamp-response-headings (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest my/gptel-clamp-response-headings-preserves-response-wrapper ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* Title\n"
    (my/gptel-clamp-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** Title\n"))))

(ert-deftest my/gptel-clamp-response-headings-ignores-user-content ()
  (my/gptel-test--with-org-buffer
      "*** Prompt\n@user\n* User heading\n\n*** Response\n@assistant\n* Model heading\n"
    (my/gptel-clamp-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Prompt\n@user\n* User heading\n\n*** Response\n@assistant\n**** Model heading\n"))))

(ert-deftest my/gptel-clamp-response-headings-multiple-headings ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* A\n** B\n*** C\n**** D\n"
    (my/gptel-clamp-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** A\n**** B\n**** C\n**** D\n"))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
