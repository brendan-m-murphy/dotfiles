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

(defun my/gptel-test--response-region (start-marker end-marker)
  "Return plist suitable for gptel callback INFO."
  (list :beg start-marker :end end-marker))

(ert-deftest my/gptel-normalize-response-headings-relative-normalization ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n@assistant:\n\n** A\n*** B\n"
    (search-forward "** A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings nil
                                            (my/gptel-test--response-region beg end)))
    (should (string-match-p
             (regexp-quote "**** A\n***** B\n")
             (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-repairs-malformed-prefixes ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n@assistant:\n\n**/ A\n"
    (search-forward "**/ A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings nil
                                            (my/gptel-test--response-region beg end)))
    (should (string-match-p
             (regexp-quote "**** A\n")
             (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-no-headings-no-change ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n@assistant:\n\nPlain text.\n"
    (let ((before (buffer-string)))
      (search-forward "Plain text.")
      (let ((beg (line-beginning-position))
            (end (point-max)))
        (my/gptel-normalize-response-headings nil
                                              (my/gptel-test--response-region beg end)))
      (should (equal before (buffer-string))))))

(ert-deftest my/gptel-normalize-response-headings-does-not-modify-outside-region ()
  (my/gptel-test--with-org-buffer
      "* Outside\n** Topic\n*** Conversation\n@assistant:\n\n** A\n*** B\n\n* Tail\n"
    (let ((outside-before nil)
          (outside-after nil))
      (search-forward "** A")
      (let ((beg (match-beginning 0))
            (end (progn (search-forward "\n* Tail") (match-beginning 0)))
            beg-marker
            end-marker)
        (setq beg-marker (copy-marker beg)
              end-marker (copy-marker end t))
        (setq outside-before
              (concat (buffer-substring-no-properties (point-min) beg)
                      "<REGION>"
                      (buffer-substring-no-properties end (point-max))))
        (my/gptel-normalize-response-headings nil
                                              (list :beg beg-marker :end end-marker))
        (setq outside-after
              (concat (buffer-substring-no-properties
                       (point-min)
                       (marker-position beg-marker))
                      "<REGION>"
                      (buffer-substring-no-properties
                       (marker-position end-marker)
                       (point-max))))
        (set-marker beg-marker nil)
        (set-marker end-marker nil))
      (should (equal outside-before outside-after)))))



(ert-deftest my/gptel-normalize-response-headings-never-promotes-upward ()
  (my/gptel-test--with-org-buffer
      "** Topic
*** Conversation
@assistant:

***** Deep
"
    (search-forward "***** Deep")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings nil
                                            (my/gptel-test--response-region beg end)))
    (should (string-match-p (regexp-quote "***** Deep
")
                            (buffer-string)))))

(ert-deftest my/gptel-base-depth-uses-parent-heading ()
  (my/gptel-test--with-org-buffer
      "* L1 Parent
** Topic
*** Conversation
@assistant:

** A
"
    (search-forward "** A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings
       nil
       (my/gptel-test--response-region beg end)))
    ;; Must normalize relative to the level-3 conversation, not the level-2 topic
    (should (string-match-p "**** A" (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-signals-on-depth-invariant-violation ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n* Too shallow\n"
    (search-forward "* Too shallow")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (should-error
       (my/gptel-normalize-response-headings nil
                                             (my/gptel-test--response-region beg end))))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
