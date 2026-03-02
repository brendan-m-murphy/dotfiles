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

(ert-deftest my/gptel-normalize-response-headings-relative-normalization ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n@assistant:\n\n** A\n*** B\n"
    (search-forward "** A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))
    (should (string-match-p
             (regexp-quote "**** A\n***** B\n")
             (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-repairs-malformed-prefixes ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n@assistant:\n\n**/ A\n"
    (search-forward "**/ A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))
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
        (my/gptel-normalize-response-headings beg end))
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
        (my/gptel-normalize-response-headings beg-marker end-marker)
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
      (my/gptel-normalize-response-headings beg end))
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
      (my/gptel-normalize-response-headings beg end))
    ;; Must normalize relative to the level-3 conversation, not the level-2 topic
    (should (string-match-p "**** A" (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-no-parent-heading-safe-noop ()
  (my/gptel-test--with-org-buffer
      "Plain prelude\n\n** A\n*** B\n"
    (let ((before (buffer-string)))
      (search-forward "** A")
      (let ((beg (match-beginning 0))
            (end (point-max)))
        (my/gptel-normalize-response-headings beg end))
      (should (equal before (buffer-string))))))

(ert-deftest my/gptel-normalize-response-headings-clamps-target-min-to-4 ()
  (my/gptel-test--with-org-buffer
      "* L1 Parent\n\n** A\n*** B\n"
    (search-forward "** A")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))
    (should (string-match-p
             (regexp-quote "**** A\n***** B\n")
             (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-normalizes-depth-one-without-error ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Conversation\n* Too shallow\n"
    (search-forward "* Too shallow")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))
    (should (string-match-p (regexp-quote "**** Too shallow\n")
                            (buffer-string)))))

(ert-deftest my/gptel-normalize-response-headings-integration-clamps-to-conversation-depth ()
  (my/gptel-test--with-org-buffer
      "** Topic\n*** Question\n@assistant:\n\n*** Outline\n**/ Detail\n"
    (search-forward "*** Outline")
    (let ((beg (match-beginning 0))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))
    (should (string-match-p
             (regexp-quote "**** Outline\n**** Detail\n")
             (buffer-string)))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
