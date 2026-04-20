;;; my-gptel-org-workflow-tests.el --- Tests for gptel org workflow -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'org)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory load-file-name)))

(unless (featurep 'gptel)
  (provide 'gptel))

(load-file (expand-file-name "../my-gptel-tools.el"
                             (file-name-directory load-file-name)))
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

(defun my/gptel-test--topic-point ()
  "Move point to the test Topic heading."
  (goto-char (point-min))
  (re-search-forward "^\\*\\* Topic" nil t)
  (beginning-of-line))

(defmacro my/gptel-test--with-temp-dir (var &rest body)
  "Bind VAR to a temporary directory for BODY."
  (declare (indent 1))
  `(let ((,var (make-temp-file "my-gptel-org-test-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,var t))))

(defun my/gptel-test--dir (path)
  "Return PATH as a canonical directory name."
  (file-name-as-directory (file-truename path)))

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
                         (re-search-forward "^\\*\\*\\* Response$" nil t 2)
                         (line-beginning-position)))
           (second-end (point-max)))
      (my/gptel-normalize-response-headings second-beg second-end)
      (should (string-match-p "^\\* First$" (buffer-string)))
      (should (string-match-p "^\\*\\*\\*\\* Second$" (buffer-string))))))

(ert-deftest my/gptel-markdown-conversion-headings-then-normalize ()
  (let* ((md "## Title\n\n### Section\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (my/gptel-normalize-response-headings (point-min) (point-max))
      (should (string-match-p "^\\*\\*\\*\\* Title$" (buffer-string)))
      (should (string-match-p "^\\*\\*\\*\\*\\* Section$" (buffer-string))))))

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
        (should-not (string-match-p "^\\*\\*\\*\\* item" (buffer-string)))
        (should (equal before (buffer-string)))))))

(ert-deftest my/gptel-markdown-conversion-emphasis-not-heading ()
  (let* ((md "*is this bold?*\n")
         (org (gptel--convert-markdown->org md)))
    (my/gptel-test--with-org-buffer
        (concat "*** Response\n@assistant\n" org)
      (let ((before (buffer-string)))
        (my/gptel-normalize-response-headings (point-min) (point-max))
        (should (string-match-p "is this bold\\?" (buffer-string)))
        (should-not (string-match-p "^\\*\\*\\*\\* is this bold" (buffer-string)))
        (should (equal before (buffer-string)))))))

(ert-deftest my/gptel-session-scope-preserves-defaults-without-properties ()
  (my/gptel-test--with-org-buffer
      "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:END:\n"
    (my/gptel-test--topic-point)
    (let ((my/gptel-extra-roots '("/tmp/extra"))
          (my/gptel-session-roots '("/tmp/session"))
          (my/gptel-relevant-buffers '("notes.org")))
      (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () "/tmp/project")))
        (let ((scope (my/gptel--session-scope)))
          (should-not (plist-get scope :scoped-p))
          (should (equal (plist-get scope :allowed-roots)
                         (mapcar #'my/gptel-test--dir
                                 '("/tmp/project" "/tmp/extra" "/tmp/session"))))
          (should (equal (plist-get scope :relevant-buffers)
                         '("notes.org")))
          (should (equal (plist-get scope :ignore-globs)
                         my/gptel-default-ignore-globs)))))))

(ert-deftest my/gptel-session-scope-explicit-roots-only ()
  (my/gptel-test--with-temp-dir explicit-a
    (my/gptel-test--with-temp-dir explicit-b
      (my/gptel-test--with-org-buffer
          (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s %s\n:END:\n"
                  explicit-a explicit-b)
        (my/gptel-test--topic-point)
        (let ((my/gptel-extra-roots '("/tmp/extra"))
              (my/gptel-session-roots '("/tmp/session")))
          (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () "/tmp/project")))
            (let ((scope (my/gptel--session-scope)))
              (should (plist-get scope :scoped-p))
              (should (equal (plist-get scope :allowed-roots)
                             (mapcar #'my/gptel-test--dir
                                     (list explicit-a explicit-b))))
              (should-not (plist-get scope :inherit-default-roots)))))))))

(ert-deftest my/gptel-session-scope-explicit-roots-with-inherit-true ()
  (my/gptel-test--with-temp-dir explicit
    (my/gptel-test--with-org-buffer
        (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_INHERIT_DEFAULT_ROOTS: yes\n:END:\n"
                explicit)
      (my/gptel-test--topic-point)
      (let ((my/gptel-extra-roots '("/tmp/extra")))
        (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () "/tmp/project")))
          (let ((scope (my/gptel--session-scope)))
            (should (plist-get scope :inherit-default-roots))
            (should (equal (plist-get scope :allowed-roots)
                           (append (list (my/gptel-test--dir explicit))
                                   (mapcar #'my/gptel-test--dir
                                           '("/tmp/project" "/tmp/extra")))))))))))

(ert-deftest my/gptel-session-scope-explicit-roots-with-inherit-false ()
  (my/gptel-test--with-temp-dir explicit
    (my/gptel-test--with-org-buffer
        (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_INHERIT_DEFAULT_ROOTS: nil\n:END:\n"
                explicit)
      (my/gptel-test--topic-point)
      (let ((my/gptel-extra-roots '("/tmp/extra"))
            (my/gptel-session-roots '("/tmp/session")))
        (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () "/tmp/project")))
          (let ((scope (my/gptel--session-scope)))
            (should-not (plist-get scope :inherit-default-roots))
            (should (equal (plist-get scope :allowed-roots)
                           (list (my/gptel-test--dir explicit))))))))))

(ert-deftest my/gptel-session-scope-relevant-buffers-resolve-live-buffer-name ()
  (let ((buffer (generate-new-buffer "flux_processing.org")))
    (unwind-protect
        (my/gptel-test--with-org-buffer
            "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_RELEVANT_BUFFERS: flux_processing.org\n:END:\n"
          (my/gptel-test--topic-point)
          (let ((scope (my/gptel--session-scope)))
            (should (equal (plist-get scope :relevant-buffers)
                           '("flux_processing.org")))))
      (kill-buffer buffer))))

(ert-deftest my/gptel-session-scope-relevant-buffers-resolve-live-file-path ()
  (my/gptel-test--with-temp-dir root
    (let* ((path (expand-file-name "todo.org" root))
           (buffer (find-file-noselect path)))
      (unwind-protect
          (my/gptel-test--with-org-buffer
              (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_RELEVANT_BUFFERS: %s\n:END:\n"
                      path)
            (my/gptel-test--topic-point)
            (let ((scope (my/gptel--session-scope)))
              (should (equal (plist-get scope :relevant-buffers)
                             (list (buffer-name buffer))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/gptel-split-property-entries-preserves-quoted-spaces ()
  (should (equal (my/gptel--split-property-entries
                  "pymc_model_refactor.org '*forge: openghg/openghg_inversions #375*' \"*magit-diff: ~/repo/*\"")
                 '("pymc_model_refactor.org"
                   "*forge: openghg/openghg_inversions #375*"
                   "*magit-diff: ~/repo/*"))))

(ert-deftest my/gptel-session-scope-relevant-buffers-resolve-quoted-buffer-names ()
  (let ((forge (generate-new-buffer "*forge: openghg/openghg_inversions #375*"))
        (magit (generate-new-buffer "*magit-diff: ~/Documents/openghg_inversions-wt/openghg_inversions-task-stage-e-follow-up-tidying/*")))
    (unwind-protect
        (my/gptel-test--with-org-buffer
            "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_RELEVANT_BUFFERS: '*forge: openghg/openghg_inversions #375*' \"*magit-diff: ~/Documents/openghg_inversions-wt/openghg_inversions-task-stage-e-follow-up-tidying/*\"\n:END:\n"
          (my/gptel-test--topic-point)
          (let ((scope (my/gptel--session-scope)))
            (should (equal (plist-get scope :relevant-buffers)
                           '("*forge: openghg/openghg_inversions #375*"
                             "*magit-diff: ~/Documents/openghg_inversions-wt/openghg_inversions-task-stage-e-follow-up-tidying/*")))))
      (kill-buffer forge)
      (kill-buffer magit))))

(ert-deftest my/gptel-session-scope-appends-ignore-globs-from-properties ()
  (my/gptel-test--with-temp-dir root
    (my/gptel-test--with-org-buffer
        "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_IGNORE_GLOBS: notebooks/** data/** scratch/**\n:END:\n"
      (my/gptel-test--topic-point)
      (let ((captured nil))
        (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () root))
                  ((symbol-function 'executable-find) (lambda (_cmd) t))
                  ((symbol-function 'process-file)
                   (lambda (_program _infile destination _display &rest args)
                     (setq captured args)
                     (with-current-buffer (if (bufferp destination)
                                              destination
                                            (current-buffer))
                       (insert ""))
                     0)))
          (my/gptel-tool-rg :dir root :pattern "needle")
          (should (member "!.git/**" captured))
          (should (member "!notebooks/**" captured))
          (should (member "!data/**" captured))
          (should (member "!scratch/**" captured)))))))

(ert-deftest my/gptel-glob-match-p-supports-double-star ()
  (should (my/gptel--glob-match-p ".venv/**" ".venv"))
  (should (my/gptel--glob-match-p ".venv/**" ".venv/bin/python"))
  (should (my/gptel--glob-match-p "data/**" "data/output.nc"))
  (should (my/gptel--glob-match-p "data/**" "data/nested/output.nc"))
  (should (my/gptel--glob-match-p "**/.venv/**" "src/.venv/bin/python"))
  (should-not (my/gptel--glob-match-p ".venv/**" "src/.venv/bin/python"))
  (should-not (my/gptel--glob-match-p "data/**" "metadata/output.nc")))

(ert-deftest my/gptel-list-files-excludes-ignored-paths ()
  (my/gptel-test--with-temp-dir root
    (let ((visible (expand-file-name "src/app.py" root))
          (ignored-a (expand-file-name ".venv/bin/python" root))
          (ignored-b (expand-file-name "data/nested/output.csv" root)))
      (make-directory (file-name-directory visible) t)
      (make-directory (file-name-directory ignored-a) t)
      (make-directory (file-name-directory ignored-b) t)
      (with-temp-file visible (insert "print('ok')"))
      (with-temp-file ignored-a (insert "python"))
      (with-temp-file ignored-b (insert "csv"))
      (my/gptel-test--with-org-buffer
          (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_IGNORE_GLOBS: .venv/** data/**\n:END:\n"
                  root)
        (my/gptel-test--topic-point)
        (let ((files (my/gptel-tool-list-files :dir root :glob "**/*")))
          (should (string-match-p "src/app.py" files))
          (should-not (string-match-p "\\.venv/bin/python" files))
          (should-not (string-match-p "data/nested/output.csv" files)))))))

(ert-deftest my/gptel-read-range-refuses-ignored-paths ()
  (my/gptel-test--with-temp-dir root
    (let ((ignored (expand-file-name ".venv/bin/python" root)))
      (make-directory (file-name-directory ignored) t)
      (with-temp-file ignored (insert "python"))
      (my/gptel-test--with-org-buffer
          (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_IGNORE_GLOBS: .venv/**\n:END:\n"
                  root)
        (my/gptel-test--topic-point)
        (should-error
         (my/gptel-tool-read-range :path ignored :start_line 1 :end_line 1)
         :type 'user-error)))))

(ert-deftest my/gptel-system-message-includes-session-scope-summary ()
  (my/gptel-test--with-temp-dir root
    (let ((my/gptel-relevant-buffers '("notes.org")))
      (my/gptel-test--with-org-buffer
          (format "* Outside\n** Topic Heading\n:PROPERTIES:\n:GPTEL_TOPIC: topic_id\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_IGNORE_GLOBS: .venv/**\n:END:\n"
                  root)
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Topic Heading" nil t)
        (beginning-of-line)
        (let ((message (my/gptel-system-message)))
          (should (string-match-p "Session scope: active" message))
          (should (string-match-p "Session topic: Topic Heading \\[GPTEL_TOPIC=topic_id\\]" message))
          (should (string-match-p "Inherit default roots: no" message))
          (should (string-match-p (regexp-quote (my/gptel-test--dir root)) message))
          (should (string-match-p "\\.venv/\\*\\*" message))
          (should (string-match-p "Treat ignored-glob paths as off-limits" message)))))))

(ert-deftest my/gptel-session-scope-resolves-relative-relevant-paths-from-org-buffer-dir ()
  (my/gptel-test--with-temp-dir root
    (let* ((org-file (expand-file-name "chat.org" root))
           (path (expand-file-name "notes.org" root))
           (buffer (find-file-noselect path)))
      (with-temp-file org-file (insert ""))
      (unwind-protect
          (my/gptel-test--with-org-buffer
              "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_RELEVANT_BUFFERS: notes.org\n:END:\n"
            (setq-local buffer-file-name org-file)
            (my/gptel-test--topic-point)
            (let ((scope (my/gptel--session-scope)))
              (should (equal (plist-get scope :relevant-buffers)
                             (list (buffer-name buffer))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest my/gptel-add-current-buffer-to-session-scope-updates-property ()
  (let ((buffer (generate-new-buffer "flux_processing.org")))
    (unwind-protect
        (my/gptel-test--with-org-buffer
            "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:END:\n"
          (my/gptel-test--topic-point)
          (my/gptel-add-current-buffer-to-session-scope buffer)
          (should (equal (org-entry-get nil "GPTEL_RELEVANT_BUFFERS")
                         "flux_processing.org")))
      (kill-buffer buffer))))

(ert-deftest my/gptel-add-current-buffer-to-session-scope-quotes-spaced-buffer-name ()
  (let ((buffer (generate-new-buffer "*forge: openghg/openghg_inversions #375*")))
    (unwind-protect
        (my/gptel-test--with-org-buffer
            "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:END:\n"
          (my/gptel-test--topic-point)
          (my/gptel-add-current-buffer-to-session-scope buffer)
          (should (equal (org-entry-get nil "GPTEL_RELEVANT_BUFFERS")
                         "\"*forge: openghg/openghg_inversions #375*\"")))
      (kill-buffer buffer))))

(ert-deftest my/gptel-add-session-ignore-globs-deduplicates-and-preserves-order ()
  (my/gptel-test--with-org-buffer
      "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_IGNORE_GLOBS: .venv/**\n:END:\n"
    (my/gptel-test--topic-point)
    (my/gptel-add-session-ignore-globs '(".venv/**" "data/**" "scratch/**"))
    (should (equal (org-entry-get nil "GPTEL_IGNORE_GLOBS")
                   ".venv/** data/** scratch/**"))))

(ert-deftest my/gptel-clear-session-scope-properties-removes-all-scope-properties ()
  (my/gptel-test--with-org-buffer
      "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: /tmp/root\n:GPTEL_RELEVANT_BUFFERS: buf\n:GPTEL_IGNORE_GLOBS: .venv/**\n:GPTEL_INHERIT_DEFAULT_ROOTS: yes\n:END:\n"
    (my/gptel-test--topic-point)
    (my/gptel-clear-session-scope-properties)
    (dolist (property my/gptel-session-scope-properties)
      (should-not (org-entry-get nil property)))))

(ert-deftest my/gptel-boolean-property-parsing-variants ()
  (dolist (pair '(("t" . t)
                  ("true" . t)
                  ("yes" . t)
                  ("1" . t)
                  ("nil" . nil)
                  ("false" . nil)
                  ("no" . nil)
                  ("0" . nil)))
    (should (eq (my/gptel--parse-boolean-property (car pair)) (cdr pair))))
  (should (eq (my/gptel--parse-boolean-property "maybe") :invalid)))

(ert-deftest my/gptel-delete-dups-stable-preserves-order ()
  (should (equal (my/gptel--delete-dups-stable '("a" "b" "a" "c" "b"))
                 '("a" "b" "c"))))

(ert-deftest my/gptel-session-scope-deduplicates-roots-preserving-order ()
  (my/gptel-test--with-temp-dir explicit
    (my/gptel-test--with-org-buffer
        (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s %s\n:GPTEL_INHERIT_DEFAULT_ROOTS: yes\n:END:\n"
                explicit explicit)
      (my/gptel-test--topic-point)
      (let ((my/gptel-extra-roots nil)
            (my/gptel-session-roots nil))
        (cl-letf (((symbol-function 'my/gptel--project-root) (lambda () explicit)))
          (let* ((normalized (file-name-as-directory (file-truename explicit)))
                 (scope (my/gptel--session-scope)))
            (should (equal (plist-get scope :allowed-roots)
                           (list normalized)))))))))

(ert-deftest my/gptel-describe-session-scope-renders-resolved-values ()
  (my/gptel-test--with-temp-dir root
    (let ((buffer (generate-new-buffer "notes.org")))
      (unwind-protect
          (my/gptel-test--with-org-buffer
              (format "* Outside\n** Topic\n:PROPERTIES:\n:GPTEL_TOPIC: demo\n:GPTEL_ALLOWED_ROOTS: %s\n:GPTEL_RELEVANT_BUFFERS: notes.org\n:GPTEL_IGNORE_GLOBS: scratch/**\n:GPTEL_INHERIT_DEFAULT_ROOTS: no\n:END:\n"
                      root)
            (my/gptel-test--topic-point)
            (cl-letf (((symbol-function 'display-buffer) (lambda (buf &rest _) buf)))
              (my/gptel-describe-session-scope)
              (with-current-buffer "*gptel-session-scope*"
                (should (string-match-p "Scoped properties active: yes" (buffer-string)))
                (should (string-match-p "Inherit default roots: no" (buffer-string)))
                (should (string-match-p (regexp-quote (file-name-as-directory (file-truename root)))
                                        (buffer-string)))
                (should (string-match-p "notes.org" (buffer-string)))
                (should (string-match-p "scratch/\\*\\*" (buffer-string))))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))
        (when (get-buffer "*gptel-session-scope*")
          (kill-buffer "*gptel-session-scope*"))))))

(provide 'my-gptel-org-workflow-tests)
;;; my-gptel-org-workflow-tests.el ends here
