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

(unless (fboundp 'gptel--convert-markdown->org)
  ;; Lightweight fallback used only when gptel is unavailable in test env.
  ;; CI with real gptel will use the real converter.
  (defun gptel--convert-markdown->org (md)
    (let ((s md))
      (setq s (replace-regexp-in-string
               "```\\([[:alnum:]_+-]+\\)\\n\\([\\s\\S]*?\\)```"
               "#+begin_src \\1\n\\2#+end_src"
               s))
      (setq s (replace-regexp-in-string
               "```\\n\\([\\s\\S]*?\\)```"
               "#+begin_example\n\\1#+end_example"
               s))
      (setq s (replace-regexp-in-string "^### " "*** " s))
      (setq s (replace-regexp-in-string "^## " "** " s))
      (setq s (replace-regexp-in-string "^- " "- " s))
      s)))

(ert-deftest my/gptel-normalize-response-headings-preserves-relative-depth ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* Title\n** Section\n*** Subsection\n"
    (my/gptel-normalize-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** Title\n***** Section\n****** Subsection\n"))))

(ert-deftest my/gptel-normalize-response-headings-already-valid-unchanged ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n**** Title\n***** Section\n"
    (let ((before (buffer-string)))
      (my/gptel-normalize-response-headings (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest my/gptel-normalize-response-headings-wrapper-protection ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* Title\n"
    (my/gptel-normalize-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** Title\n"))))

(ert-deftest my/gptel-normalize-response-headings-ignores-user-content ()
  (my/gptel-test--with-org-buffer
      "*** Prompt\n@user\n* User heading\n\n*** Response\n@assistant\n* Model heading\n"
    (my/gptel-normalize-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Prompt\n@user\n* User heading\n\n*** Response\n@assistant\n**** Model heading\n"))))

(ert-deftest my/gptel-normalize-response-headings-relative-depth-mixed-levels ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n** A\n**** B\n*** C\n"
    (my/gptel-normalize-response-headings (point-min) (point-max))
    (should (equal (buffer-string)
                   "*** Response\n@assistant\n**** A\n****** B\n***** C\n"))))

(ert-deftest my/gptel-normalize-response-headings-no-headings-no-change ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\nPlain paragraph.\n"
    (let ((before (buffer-string)))
      (my/gptel-normalize-response-headings (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest my/gptel-normalize-response-headings-region-scoping ()
  (my/gptel-test--with-org-buffer
      "*** Response\n@assistant\n* First\n\n*** Response\n@assistant\n* Second\n"
    (let* ((second-beg (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "^\*\*\* Response$" nil t 2)
                         (line-beginning-position)))
           (second-end (point-max)))
      (my/gptel-normalize-response-headings second-beg second-end)
      (should (string-match-p "^\* First$" (buffer-string)))
      (should (string-match-p "^\*\*\*\* Second$" (buffer-string))))))

(ert-deftest my/gptel-markdown-conversion-headings-then-normalize ()
  (let* ((md "## Title\n\n### Section\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (my/gptel-normalize-response-headings (point-min) (point-max))
      (should (string-match-p "^\*\*\*\* Title$" (buffer-string)))
      (should (string-match-p "^\*\*\*\*\* Section$" (buffer-string))))))

(ert-deftest my/gptel-markdown-conversion-fenced-code-with-language ()
  (let* ((md "```python\nprint(\"hi\")\n```\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (let ((before (buffer-string)))
        (my/gptel-normalize-response-headings (point-min) (point-max))
        (should (string-match-p "#\\+begin_src python" (buffer-string)))
        (should (string-match-p "print(\\\"hi\\\")" (buffer-string)))
        (should (equal before (buffer-string)))))))

(ert-deftest my/gptel-markdown-conversion-fenced-code-without-language ()
  (let* ((md "```\nprint(\"hi\")\n```\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (let ((before (buffer-string)))
        (my/gptel-normalize-response-headings (point-min) (point-max))
        (should (string-match-p "print(\\\"hi\\\")" (buffer-string)))
        (should (equal before (buffer-string)))))))

(ert-deftest my/gptel-markdown-conversion-list-not-heading ()
  (let* ((md "- item\n- item\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (let ((before (buffer-string)))
        (my/gptel-normalize-response-headings (point-min) (point-max))
        (should (string-match-p "^- item" (buffer-string)))
        (should-not (string-match-p "^\*\*\*\* item" (buffer-string)))
        (should (equal before (buffer-string)))))))

(ert-deftest my/gptel-markdown-conversion-emphasis-not-heading ()
  (let* ((md "*is this bold?*\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (let ((before (buffer-string)))
        (my/gptel-normalize-response-headings (point-min) (point-max))
        (should (string-match-p "is this bold\\?" (buffer-string)))
        (should-not (string-match-p "^\*\*\*\* is this bold" (buffer-string)))
        (should (equal before (buffer-string)))))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
