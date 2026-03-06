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

(ert-deftest my/gptel-base-depth-ignores-headings-inside-region ()
  "Base depth must be computed from the nearest heading strictly before BEG,
not from a heading inside the response region."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Root\n"
     "** Topic\n"
     "*** Question\n"
     "@assistant\n"
     "*** Outline\n"      ;; This is INSIDE region and must NOT be used as base
     "- item\n")

    ;; Region starts at the Outline heading
    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "*** Outline")
                  (match-beginning 0)))
           (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    ;; After normalization, Outline must become depth 4, not 5
    (goto-char (point-min))
    (search-forward "Outline")
    (beginning-of-line)
    (should (looking-at "**** Outline"))))

(ert-deftest my/gptel-normalize-response-headings-no-extra-indent ()
  "Assistant headings should normalize to base-depth+1 (min 4),
not deeper."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Root\n"
     "** Topic\n"
     "*** Question\n"
     "@assistant\n"
     "*** Outline\n")

    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "*** Outline")
                  (match-beginning 0)))
           (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    (goto-char (point-min))
    (search-forward "Outline")
    (beginning-of-line)

    ;; Must be exactly 4 stars
    (should (looking-at "\\*\\{4\\} Outline"))
    ;; Must NOT be 5
    (should-not (looking-at "\\*\\{5\\} Outline"))))

(ert-deftest my/gptel-normalize-response-headings-after-assistant-marker ()
  "Ensure base depth detection works when region begins
after an @assistant line."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Root\n"
     "** Topic\n"
     "*** Question\n"
     "@assistant\n\n"
     "*** Outline\n")

    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "*** Outline")
                  (match-beginning 0)))
           (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    (goto-char (point-min))
    (search-forward "Outline")
    (beginning-of-line)

    (should (looking-at "\\*\\{4\\} Outline"))))

(ert-deftest my/gptel-base-depth-strictly-before-beg ()
  "A heading starting exactly at BEG must not be used
to compute base depth."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Root\n"
     "** Topic\n"
     "*** Question\n"
     "*** Outline\n") ;; region starts here

    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "*** Outline")
                  (match-beginning 0)))
           (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    (goto-char (point-min))
    (search-forward "Outline")
    (beginning-of-line)

    ;; Must normalize relative to *** Question (depth 3)
    ;; so target-min = 4
    (should (looking-at "\\*\\{4\\} Outline"))))

(ert-deftest my/gptel-normalize-response-headings-no-parent-enforces-minimum ()
  "If no parent heading exists above region,
assistant headings must still be clamped to depth >= 4."
  (with-temp-buffer
    (org-mode)
    (insert "*** Outline\n")

    (let ((beg (point-min))
          (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    (goto-char (point-min))
    (should (looking-at "\\*\\{4\\} Outline"))))

(ert-deftest my/gptel-normalize-response-headings-realistic-conversation ()
  "Simulate real conversation layout under topic and question."
  (with-temp-buffer
    (org-mode)
    (insert
     "* Root\n"
     "** Topic\n"
     "*** Question\n"
     "@user\n"
     "Prompt text\n\n"
     "@assistant\n"
     "*** Outline\n"
     "**/ Broken\n")

    ;; Region should start at first assistant heading
    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "*** Outline")
                  (match-beginning 0)))
           (end (point-max)))
      (my/gptel-normalize-response-headings beg end))

    ;; Debug print
    (message "%s" (buffer-string))

    ;; Outline must be depth 4
    (goto-char (point-min))
    (search-forward "Outline")
    (beginning-of-line)
    (should (looking-at "\\*\\{4\\} Outline"))

    ;; Broken prefix repaired and clamped
    (search-forward "Broken")
    (beginning-of-line)
    (should (looking-at "\\*\\{4\\} Broken"))))


(ert-deftest my/gptel-wrap-response-inserts-response-heading-before-assistant ()
  "Wrap inserts a level-3 Response heading immediately before @assistant."
  (my/gptel-test--with-org-buffer
      "*** 2026-03-06 — Question
@user
hello

@assistant
hello
"
    (let ((beg (save-excursion
                 (goto-char (point-min))
                 (search-forward "@assistant")
                 (line-beginning-position)))
          (end (point-max)))
      (my/gptel-wrap-response nil (list :beg beg :end end)))
    (should (string-match-p
             (regexp-quote
              "*** 2026-03-06 — Question
@user
hello

*** ")
             (buffer-string)))
    (goto-char (point-min))
    (search-forward "— Response")
    (forward-line 1)
    (should (looking-at "@assistant"))))

(ert-deftest my/gptel-wrap-response-does-not-add-extra-assistant-marker ()
  "Wrap should keep exactly one @assistant marker in the wrapped region."
  (my/gptel-test--with-org-buffer
      "*** 2026-03-06 — Question
@user
hello

@assistant
hello
"
    (let ((beg (save-excursion
                 (goto-char (point-min))
                 (search-forward "@assistant")
                 (line-beginning-position)))
          (end (point-max)))
      (my/gptel-wrap-response nil (list :beg beg :end end)))
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward "^@assistant\(?::\)?[ \t]*$" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest my/gptel-wrap-and-normalize-keep-response-heading-at-level-3 ()
  "Response heading inserted by wrap must not be normalized down to level 4."
  (my/gptel-test--with-org-buffer
      "* Root
** Topic
*** 2026-03-06 — Question
@user
hello

@assistant
*** Outline
"
    (let* ((beg (save-excursion
                  (goto-char (point-min))
                  (search-forward "@assistant")
                  (line-beginning-position)))
           (end (point-max)))
      (my/gptel-wrap-response nil (list :beg beg :end end))
      (my/gptel-normalize-response-headings beg end))
    (goto-char (point-min))
    (should (re-search-forward "^\*\*\* .* — Response$" nil t))
    (beginning-of-line)
    (should-not (looking-at "^\*\*\*\* .* — Response$"))
    (forward-line 1)
    (should (looking-at "@assistant"))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
