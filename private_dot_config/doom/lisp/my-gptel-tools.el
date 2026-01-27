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

(defvar my/gptel-session-roots nil
  "Ephemeral allowed roots for gptel tools (current Emacs session only).")

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

(defun my/gptel--tru-dir (p)
  "Canonicalize P as a directory truename with trailing slash."
  (file-name-as-directory (file-truename (expand-file-name p))))

(defun my/gptel--allowed-roots ()
  "Compute effective allowed roots: project root + extra roots + session roots.
All roots are canonicalized via `file-truename` and normalized with trailing slash."
  (let ((roots nil))
    (when-let ((pr (my/gptel--project-root)))
      (push (my/gptel--tru-dir pr) roots))
    (dolist (r my/gptel-extra-roots)
      (when r (push (my/gptel--tru-dir r) roots)))
    (dolist (r my/gptel-session-roots)
      (when r (push (my/gptel--tru-dir r) roots)))
    (cl-delete-duplicates (delq nil roots) :test #'string-equal)))

;; (defun my/gptel--allowed-path-p (path)
;;   "Return non-nil if PATH is under one of the allowed roots."
;;   (let* ((tru (file-truename (expand-file-name path)))
;;          (roots (my/gptel--allowed-roots)))
;;     (cl-some (lambda (r) (string-prefix-p r tru)) roots)))

(defun my/gptel--allowed-path-p (path)
  "Return non-nil if PATH is under one of the allowed roots."
  (let* ((expanded (expand-file-name path))
         ;; Normalize directories with trailing slash so prefix tests work.
         (tru (if (file-directory-p expanded)
                  (file-name-as-directory (file-truename expanded))
                (file-truename expanded)))
         (roots (my/gptel--allowed-roots)))
    (cl-some (lambda (r) (string-prefix-p r tru)) roots)))


;; (defun my/gptel--assert-allowed (path)
;;   "Signal a `user-error` if PATH is outside allowed roots."
;;   (unless (my/gptel--allowed-path-p path)
;;     (user-error "Path not allowed: %s (allowed roots: %s)"
;;                 (file-truename (expand-file-name path))
;;                 (string-join (my/gptel--allowed-roots) ", ")))
;;   path)

(defun my/gptel--assert-allowed (path)
  "Signal a `user-error` if PATH is outside allowed roots."
  (let* ((expanded (expand-file-name path))
         ;; Normalize directories with trailing slash so prefix tests work.
         (tru (if (file-directory-p expanded)
                  (file-name-as-directory (file-truename expanded))
                (file-truename expanded))))
    (unless (cl-some (lambda (r) (string-prefix-p r tru))
                     (my/gptel--allowed-roots))
      (user-error "Path not allowed: %s (allowed roots: %s)"
                  tru (string-join (my/gptel--allowed-roots) ", "))))
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

;; (defun my/gptel--default-dir ()
;;   "Default directory for searches: project root or `default-directory`."
;;   (file-name-as-directory
;;    (file-truename (or (my/gptel--project-root) default-directory))))

(defun my/gptel--default-dir ()
  "Default directory for searches: project root, else an allowed root, else `default-directory`."
  (file-name-as-directory
   (file-truename
    (or (my/gptel--project-root)
        (car (my/gptel--allowed-roots))
        default-directory))))

;;; Interactive convenience (for you, not for the model)

(defun my/gptel-add-extra-root (dir)
  "Add DIR to `my/gptel-extra-roots` (interactive convenience, persistent via Customize)."
  (interactive "DAdd extra root directory: ")
  (setq dir (my/gptel--tru-dir dir))
  (add-to-list 'my/gptel-extra-roots dir)
  (customize-save-variable 'my/gptel-extra-roots my/gptel-extra-roots)
  (message "Added extra root (persistent): %s" dir))

(defun my/gptel-add-session-root (dir)
  "Add DIR to `my/gptel-session-roots` (session-only; frictionless for one-offs)."
  (interactive "DAllow root for this session: ")
  (setq dir (my/gptel--tru-dir dir))
  (add-to-list 'my/gptel-session-roots dir)
  (message "Added session root: %s" dir))

(defun my/gptel-clear-session-roots ()
  "Clear all session roots."
  (interactive)
  (setq my/gptel-session-roots nil)
  (message "Cleared all session roots"))

(defun my/gptel-show-roots ()
  "Show current allowed roots (project + extra + session) in the minibuffer."
  (interactive)
  (message "Allowed roots: %s"
           (string-join (my/gptel--allowed-roots) ", ")))

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
             (:name "n"    :type "number" :optional t)))))

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
