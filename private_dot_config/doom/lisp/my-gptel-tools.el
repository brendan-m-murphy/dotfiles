;;; lisp/my-gptel-tools.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup my/gptel-tools nil
  "Token-efficient local tools for gptel."
  :group 'gptel)

(defcustom my/gptel-extra-roots nil
  "Extra allowed roots for gptel tools.
Each entry should be a directory. Paths are canonicalized via `file-truename`."
  :type '(repeat directory))

(defcustom my/gptel-max-lines 250
  "Hard cap on lines returned by any tool."
  :type 'integer)

(defcustom my/gptel-max-bytes (* 64 1024)
  "Hard cap on bytes read from disk for any tool."
  :type 'integer)

(defcustom my/gptel-rg-max-hits 200
  "Hard cap on number of ripgrep hits returned."
  :type 'integer)

(defun my/gptel--project-root ()
  "Best-effort project root using projectile, then project.el, else nil."
  (cond
   ;; Projectile (common in Doom)
   ((and (boundp 'projectile-project-root)
         (fboundp 'projectile-project-root))
    (ignore-errors (projectile-project-root)))
   ;; project.el fallback
   ((and (fboundp 'project-current)
         (fboundp 'project-root))
    (when-let ((p (ignore-errors (project-current nil))))
      (ignore-errors (project-root p))))
   (t nil)))

(defun my/gptel--allowed-roots ()
  "Compute effective allowed roots: project root (if any) + extra roots."
  (let ((roots (copy-sequence my/gptel-extra-roots)))
    (when-let ((pr (my/gptel--project-root)))
      (push pr roots))
    ;; Canonicalize, ensure directories, dedupe
    (setq roots (delq nil (mapcar (lambda (d)
                                    (when (and d (file-directory-p d))
                                      (file-name-as-directory (file-truename d))))
                                  roots)))
    (cl-delete-duplicates roots :test #'string-equal)))

(defun my/gptel--allowed-path-p (path)
  (let* ((tru (file-truename path))
         (roots (my/gptel--allowed-roots)))
    (cl-some (lambda (r) (string-prefix-p r tru)) roots)))

(defun my/gptel--assert-allowed (path)
  (unless (my/gptel--allowed-path-p path)
    (user-error "Path not allowed: %s (allowed roots: %s)"
                path
                (string-join (my/gptel--allowed-roots) ", ")))
  path)

(defun my/gptel--assert-text-file (path)
  ;; Very light heuristic: reject huge/binary-ish by checking for NUL in prefix.
  (with-temp-buffer
    (insert-file-contents path nil 0 (min my/gptel-max-bytes 4096))
    (when (string-match-p "\0" (buffer-string))
      (user-error "Refusing binary file: %s" path))))

(defun my/gptel--read-lines (path start-line end-line)
  "Read inclusive line range [start-line, end-line] with caps."
  (setq start-line (max 1 (or start-line 1)))
  (setq end-line (max start-line (or end-line (+ start-line 40))))
  (let* ((want-lines (min my/gptel-max-lines (1+ (- end-line start-line)))))
    (my/gptel--assert-allowed path)
    (my/gptel--assert-text-file path)
    (with-temp-buffer
      ;; Read at most max-bytes, to avoid pathological files.
      (insert-file-contents path nil 0 my/gptel-max-bytes)
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((beg (point)))
        (forward-line want-lines)
        (buffer-substring-no-properties beg (point))))))

(defun my/gptel--default-dir ()
  "Default directory for searches: project root or `default-directory`."
  (file-name-as-directory
   (file-truename (or (my/gptel--project-root) default-directory))))

;;; Interactive convenience (for you, not for the model)

(defun my/gptel-add-extra-root (dir)
  "Add DIR to `my/gptel-extra-roots` (interactive convenience)."
  (interactive "DAdd extra root directory: ")
  (setq dir (file-name-as-directory (file-truename dir)))
  (add-to-list 'my/gptel-extra-roots dir)
  (customize-save-variable 'my/gptel-extra-roots my/gptel-extra-roots)
  (message "Added extra root: %s" dir))

;;; Tool implementations

(defun my/gptel-tool-list-files (&rest args)
  "List files under :dir matching :glob. Returns relative paths."
  (let* ((dir (file-name-as-directory
               (file-truename (expand-file-name (or (plist-get args :dir)
                                                    (my/gptel--default-dir))))))
         (glob (or (plist-get args :glob) "**/*"))
         (max (min 500 (or (plist-get args :max) 200))))
    (my/gptel--assert-allowed dir)
    (let* ((files (file-expand-wildcards (expand-file-name glob dir) t))
           (files (cl-remove-if #'file-directory-p files))
           (files (cl-subseq files 0 (min max (length files)))))
      (mapconcat (lambda (f) (file-relative-name f dir)) files "\n"))))

(defun my/gptel-tool-rg (&rest args)
  "Ripgrep search under :dir for :pattern; optional :glob; capped."
  (let* ((dir (file-name-as-directory
               (file-truename (expand-file-name (or (plist-get args :dir)
                                                    (my/gptel--default-dir))))))
         (pattern (or (plist-get args :pattern) (user-error "Missing :pattern")))
         (glob (plist-get args :glob))
         (max (min my/gptel-rg-max-hits (or (plist-get args :max) 120))))
    (my/gptel--assert-allowed dir)
    (unless (executable-find "rg")
      (user-error "ripgrep (rg) not found on PATH"))
    (let* ((default-directory dir)
           ;; Keep flags tight: no arbitrary injection, no --pcre2, etc.
           (cmd (append (list "rg" "--no-heading" "--line-number" "--color" "never"
                              "--max-count" (number-to-string max)
                              "--hidden" "--glob" "!.git/*")
                        (when glob (list "--glob" glob))
                        (list pattern "."))))
      (string-trim
       (with-temp-buffer
         (apply #'process-file (car cmd) nil t nil (cdr cmd))
         (buffer-string))))))

(defun my/gptel-tool-read-range (&rest args)
  "Read file line range [start_line,end_line] (inclusive), capped."
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (start (plist-get args :start_line))
         (end (plist-get args :end_line)))
    (my/gptel--read-lines path start end)))

(defun my/gptel-tool_head (&rest args)
  "Read first :n lines of :path, capped."
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (n (min my/gptel-max-lines (or (plist-get args :n) 60))))
    (my/gptel--read-lines path 1 n)))

(defun my/gptel-tool_tail (&rest args)
  "Read last :n lines of :path, capped (from first max-bytes chunk)."
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (n (min my/gptel-max-lines (or (plist-get args :n) 60))))
    (my/gptel--assert-allowed path)
    (my/gptel--assert-text-file path)
    (with-temp-buffer
      (insert-file-contents path nil 0 my/gptel-max-bytes)
      (let* ((lines (split-string (buffer-string) "\n"))
             (take (last lines (min n (length lines)))))
        (mapconcat #'identity take "\n")))))

;;; Registration

(defun my/gptel-register-tools ()
  "Register local tools with gptel.
Call this from an (after! gptel ...) block."
  (when (fboundp 'gptel-make-tool)
    (gptel-make-tool
     :name "list_files"
     :function #'my/gptel-tool-list-files
     :description "List files under dir matching glob. Returns paths only."
     :args '((:name "dir"  :type "string" :optional t)
             (:name "glob" :type "string" :optional t)
             (:name "max"  :type "number" :optional t)))

    (gptel-make-tool
     :name "rg"
     :function #'my/gptel-tool-rg
     :description "Ripgrep search under dir for pattern; optional glob; capped. Returns file:line:match."
     :args '((:name "dir"     :type "string" :optional t)
             (:name "pattern" :type "string")
             (:name "glob"    :type "string" :optional t)
             (:name "max"     :type "number" :optional t)))

    (gptel-make-tool
     :name "read_range"
     :function #'my/gptel-tool-read-range
     :description "Read file line range [start_line,end_line] (inclusive), capped."
     :args '((:name "path"       :type "string")
             (:name "start_line" :type "number" :optional t)
             (:name "end_line"   :type "number" :optional t)))

    (gptel-make-tool
     :name "head"
     :function #'my/gptel-tool_head
     :description "Read first n lines of path, capped."
     :args '((:name "path" :type "string")
             (:name "n"    :type "number" :optional t)))

    (gptel-make-tool
     :name "tail"
     :function #'my/gptel-tool_tail
     :description "Read last n lines of path, capped (from first max-bytes chunk)."
     :args '((:name "path" :type "string")
             (:name "n"    :type "number" :optional t)))
    )
  )

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
