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
  ;; TEMPORARY CI fallback: copy gptel's markdown->org converter here
  ;; because CI test bootstrap currently provides only `(provide 'gptel)`.
  ;; Remove this once CI loads real gptel in the test environment.
  (defun gptel--convert-markdown->org (str)
    "Convert string STR from markdown to org markup.

This is a very basic converter that handles only a few markup
elements."
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "`+\\|\\*\\{1,2\\}\\|_\\|^#+" nil t)
        (pcase (match-string 0)
          ;; Handle backticks
          ((and (guard (eq (char-before) ?`)) ticks)
           (gptel--replace-source-marker (length ticks))
           (save-match-data
             (catch 'block-end
               (while (search-forward ticks nil t)
                 (unless (or (eq (char-before (match-beginning 0)) ?`)
                             (eq (char-after) ?`))
                   (gptel--replace-source-marker (length ticks) 'end)
                   (throw 'block-end nil))))))
          ;; Handle headings
          ((and (guard (eq (char-before) ?#)) heading)
           (cond
            ((looking-at "[[:space:]]") ;Handle headings
             (delete-region (line-beginning-position) (point))
             (insert (make-string (length heading) ?*)))
            ((looking-at "\\+begin_src") ;Overeager LLM switched to using Org src blocks
             (save-match-data (re-search-forward "^#\\+end_src" nil t)))))
          ;; Handle emphasis
          ("**" (cond
                 ((looking-back "\\(?:[[:word:][:punct:]\n]\\|\\s-\\)\\*\\{2\\}"
                                (max (- (point) 3) (point-min)))
                  (delete-char -1))))
          ("*"
           (cond
            ((save-match-data
               (and (or (= (point) 2)
                        (looking-back "\\(?:[[:space:]]\\|\\s-\\)\\(?:_\\|\\*\\)"
                                      (max (- (point) 2) (point-min))))
                    (not (looking-at "[[:space:]]\\|\\s-"))))
             ;; Possible beginning of emphasis
             (and
              (save-excursion
                (when (and (re-search-forward (regexp-quote (match-string 0))
                                              (line-end-position) t)
                           (looking-at "[[:space:][:punct:]]\\|\\s-")
                           (not (looking-back "\\(?:[[:space:]]\\|\\s-\\)\\(?:_\\|\\*\\)"
                                              (max (- (point) 2) (point-min)))))
                  (delete-char -1) (insert "/") t))
              (progn (delete-char -1) (insert "/"))))
            ((save-excursion
               (ignore-errors (backward-char 2))
               (or (and (bobp) (looking-at "\\*[[:space:]]"))
                   (looking-at "\\(?:$\\|\\`\\)\n\\*[[:space:]]")))
             ;; Bullet point, replace with hyphen
             (delete-char -1) (insert "-"))))))
      (buffer-string)))

  (defun gptel--replace-source-marker (num-ticks &optional end)
    "Replace markdown style backticks with Org equivalents.

NUM-TICKS is the number of backticks being replaced.  If END is
true these are \"ending\" backticks.

This is intended for use in the markdown to org stream converter."
    (let ((from (match-beginning 0)))
      (delete-region from (point))
      (if (and (= num-ticks 3)
               (save-excursion (beginning-of-line)
                               (skip-chars-forward " \t")
                               (eq (point) from)))
          (insert (if end "#+end_src" "#+begin_src "))
        (insert "=")))))


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
