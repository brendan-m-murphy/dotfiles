;;; lisp/my-gptel-tools.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defgroup my/gptel-tools nil
  "Token-efficient local tools for gptel."
  :group 'gptel)

(defcustom my/gptel-extra-roots nil
  "Extra allowed roots for gptel tools.
Each entry should be a directory. Paths are canonicalized via `file-truename'."
  :type '(repeat directory))

(defvar my/gptel-session-roots nil
  "Ephemeral allowed roots for gptel tools (current Emacs session only).")

;; These are intentionally buffer-local.  Multiple chat buffers can still point
;; at the same repo; v1 accepts that tradeoff and relies on the first-write
;; clean-worktree check to catch most accidental overlap.
(defvar-local my/gptel-write-root nil
  "Active write root for the current gptel conversation buffer.")

(defvar-local my/gptel-write-root-source nil
  "Where `my/gptel-write-root' came from in the current buffer.")

(defvar-local my/gptel-edit-session-started nil
  "Non-nil after the first successful mutation in the current buffer.")

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
   ((and (boundp 'projectile-project-root)
         (fboundp 'projectile-project-root))
    (ignore-errors (projectile-project-root)))
   ((and (fboundp 'project-current)
         (fboundp 'project-root))
    (when-let ((p (ignore-errors (project-current nil))))
      (ignore-errors (project-root p))))
   (t nil)))

(defun my/gptel--assert-local-path (path)
  "Reject remote/TRAMP PATH with a clear error."
  (when (file-remote-p (expand-file-name path))
    (user-error "Remote/TRAMP paths are not supported: %s" path)))

(defun my/gptel--tru-dir (path)
  "Canonicalize PATH as an existing directory truename with trailing slash."
  (my/gptel--assert-local-path path)
  (file-name-as-directory (file-truename (expand-file-name path))))

(defun my/gptel--nearest-existing-ancestor (path)
  "Return the nearest existing ancestor of PATH."
  (let ((current (expand-file-name path)))
    (while (and current (not (file-exists-p current)))
      (let ((parent (file-name-directory (directory-file-name current))))
        (setq current
              (unless (or (null parent) (string-equal parent current))
                (directory-file-name parent)))))
    (or current default-directory)))

(defun my/gptel--canonical-path (path &optional directoryp)
  "Return canonical absolute PATH, even if the final path does not exist.
If DIRECTORYP is non-nil, normalize with a trailing slash."
  (my/gptel--assert-local-path path)
  (let* ((expanded (expand-file-name path))
         (existing (my/gptel--nearest-existing-ancestor expanded))
         (true-existing (file-truename existing))
         (relative (file-relative-name expanded existing))
         (resolved (expand-file-name relative true-existing)))
    (if directoryp
        (file-name-as-directory resolved)
      resolved)))

(defun my/gptel--path-within-p (path root)
  "Return non-nil if PATH is inside ROOT."
  (string-prefix-p (my/gptel--canonical-path root t)
                   (my/gptel--canonical-path path
                                             (file-directory-p
                                              (expand-file-name path)))))

(defun my/gptel--allowed-roots ()
  "Compute effective allowed roots: project root + extra roots + session roots."
  (let ((roots nil))
    (when-let ((project-root (my/gptel--project-root)))
      (push (my/gptel--tru-dir project-root) roots))
    (dolist (root my/gptel-extra-roots)
      (when root
        (push (my/gptel--tru-dir root) roots)))
    (dolist (root my/gptel-session-roots)
      (when root
        (push (my/gptel--tru-dir root) roots)))
    (cl-delete-duplicates (delq nil roots) :test #'string-equal)))

(defun my/gptel--allowed-path-p (path)
  "Return non-nil if PATH is under one of the allowed roots."
  (condition-case nil
      (let* ((expanded (expand-file-name path))
             (tru (my/gptel--canonical-path expanded (file-directory-p expanded)))
             (roots (my/gptel--allowed-roots)))
        (cl-some (lambda (root) (string-prefix-p root tru)) roots))
    (user-error nil)))

(defun my/gptel--assert-allowed (path)
  "Signal a `user-error' if PATH is outside allowed roots."
  (let* ((expanded (expand-file-name path))
         (tru (my/gptel--canonical-path expanded (file-directory-p expanded))))
    (unless (cl-some (lambda (root) (string-prefix-p root tru))
                     (my/gptel--allowed-roots))
      (user-error "Path not allowed: %s (allowed roots: %s)"
                  tru (string-join (my/gptel--allowed-roots) ", "))))
  path)

(defun my/gptel--assert-text-content (content)
  "Reject CONTENT that looks binary."
  (when (string-match-p "\0" content)
    (user-error "Refusing binary content")))

(defun my/gptel--assert-text-file (path)
  "Reject PATH if it looks binary."
  (with-temp-buffer
    (insert-file-contents path nil 0 (min my/gptel-max-bytes 4096))
    (when (string-match-p "\0" (buffer-string))
      (user-error "Refusing binary file: %s" path))))

(defun my/gptel--read-file-string (path)
  "Read PATH as text."
  (my/gptel--assert-text-file path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun my/gptel--write-file-string (path content)
  "Write CONTENT to PATH exactly as provided."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun my/gptel--read-lines (path start-line end-line)
  "Read inclusive line range [START-LINE, END-LINE] with caps."
  (setq start-line (max 1 (or start-line 1)))
  (setq end-line (max start-line (or end-line (+ start-line 40))))
  (let ((want-lines (min my/gptel-max-lines (1+ (- end-line start-line)))))
    (my/gptel--assert-allowed path)
    (my/gptel--assert-text-file path)
    (with-temp-buffer
      (insert-file-contents path nil 0 my/gptel-max-bytes)
      (goto-char (point-min))
      (forward-line (1- start-line))
      (let ((beg (point)))
        (forward-line want-lines)
        (buffer-substring-no-properties beg (point))))))

(defun my/gptel--default-dir ()
  "Default directory for searches: project root, else an allowed root."
  (file-name-as-directory
   (file-truename
    (or (my/gptel--project-root)
        (car (my/gptel--allowed-roots))
        default-directory))))

(defun my/gptel--git-root (path)
  "Return the git root containing PATH, or nil."
  (let* ((expanded (expand-file-name path))
         (default-directory
          (if (file-directory-p expanded)
              (my/gptel--canonical-path expanded t)
            (file-name-directory (my/gptel--canonical-path expanded))))
         (output (with-temp-buffer
                   (when (eq 0 (process-file "git" nil t nil
                                             "rev-parse" "--show-toplevel"))
                     (string-trim (buffer-string))))))
    (when (and output (not (string-empty-p output)))
      (file-name-as-directory (file-truename output)))))

(defun my/gptel--git-worktree-clean-p (root)
  "Return non-nil if ROOT has no outstanding git changes."
  (let ((default-directory (my/gptel--canonical-path root t)))
    (string-empty-p
     (with-temp-buffer
       (unless (eq 0 (process-file "git" nil t nil "status" "--porcelain"))
         (user-error "Failed to inspect git status for %s" root))
       (string-trim (buffer-string))))))

(defun my/gptel--org-write-root-property ()
  "Return the current Org write-root property, if any.
Only absolute paths are supported in v1, and property-derived roots must be
git repo roots."
  (when (derived-mode-p 'org-mode)
    (when-let* ((raw (or (org-entry-get nil "GPTEL_WRITE_ROOT" 'inherit)
                         (org-entry-get nil "REPO" 'inherit)))
                (trimmed (string-trim raw))
                ((not (string-empty-p trimmed))))
      (unless (file-name-absolute-p trimmed)
        (user-error "GPTEL_WRITE_ROOT must be an absolute path: %s" trimmed))
      (let* ((root (my/gptel--canonical-path trimmed t))
             (git-root (my/gptel--git-root root)))
        (unless (and git-root (string-equal git-root root))
          (user-error "Property-derived write roots must be git repo roots: %s"
                      trimmed))
        root))))

(defun my/gptel--set-write-root-internal (root source)
  "Set ROOT as the active write root from SOURCE.
Changing the root resets the edit-session flag."
  (my/gptel--assert-allowed root)
  (let ((tru (my/gptel--canonical-path root t)))
    (unless (and my/gptel-write-root
                 (string-equal my/gptel-write-root tru))
      (setq-local my/gptel-edit-session-started nil))
    (setq-local my/gptel-write-root tru)
    (setq-local my/gptel-write-root-source source)
    tru))

(defun my/gptel--manual-write-root (dir)
  "Normalize manually selected DIR into an active write root."
  (let* ((tru (my/gptel--canonical-path dir t))
         (git-root (my/gptel--git-root tru)))
    ;; Manual roots may be non-git, but if the path lives inside a repo we lock
    ;; to the repo root to avoid spanning multiple repos accidentally.
    (if git-root git-root tru)))

(defun my/gptel--resolve-write-root (target-path)
  "Resolve the active write root for TARGET-PATH."
  (cond
   (my/gptel-write-root
    my/gptel-write-root)
   ((let ((property-root (my/gptel--org-write-root-property)))
      (when property-root
        (my/gptel--set-write-root-internal property-root 'property))))
   ((let ((git-root (my/gptel--git-root target-path)))
      (when git-root
        (my/gptel--set-write-root-internal git-root 'target-inferred))))
   (t
    (user-error
     (concat "Write root is not set. Use a manual write root, set GPTEL_WRITE_ROOT "
             "on the current Org topic, or target a path inside a git repo.")))))

(defun my/gptel--assert-write-allowed (path write-root)
  "Signal a `user-error' if PATH is outside WRITE-ROOT."
  (my/gptel--assert-local-path path)
  (my/gptel--assert-allowed path)
  (unless (my/gptel--path-within-p path write-root)
    (user-error "Write denied: %s is outside active write root %s"
                (my/gptel--canonical-path path (file-directory-p (expand-file-name path)))
                write-root)))

(defun my/gptel--assert-clean-worktree-for-first-write (repo-root)
  "Require REPO-ROOT to be clean before the first successful mutation."
  (when (and repo-root (not my/gptel-edit-session-started))
    (unless (my/gptel--git-worktree-clean-p repo-root)
      (user-error "Refusing first write: git worktree is dirty in %s" repo-root))))

(defun my/gptel--prepare-write-target (path)
  "Resolve and validate write state for PATH."
  (let* ((expanded (expand-file-name path))
         (canonical (my/gptel--canonical-path expanded))
         (write-root (my/gptel--resolve-write-root canonical))
         (repo-root (my/gptel--git-root canonical)))
    (when (and (eq my/gptel-write-root-source 'property)
               (not repo-root))
      (user-error "Property-derived write roots must point to a git repo"))
    (my/gptel--assert-write-allowed canonical write-root)
    (my/gptel--assert-clean-worktree-for-first-write repo-root)
    (list :path canonical
          :write-root write-root
          :repo-root repo-root
          :lock-root (or repo-root write-root))))

(defun my/gptel--finish-successful-mutation (state)
  "Mark the edit session as started and lock to STATE."
  (setq-local my/gptel-write-root (plist-get state :lock-root))
  (setq-local my/gptel-edit-session-started t))

(defun my/gptel--write-file-op (path thunk)
  "Run THUNK after validating write access to PATH."
  (let ((state (my/gptel--prepare-write-target path)))
    (prog1 (funcall thunk (plist-get state :path) state)
      (my/gptel--finish-successful-mutation state))))

(defun my/gptel--replace-nth (text old new occurrence)
  "Replace OCCURRENCE of OLD with NEW in TEXT."
  (let ((start 0)
        (positions nil))
    (while (string-match (regexp-quote old) text start)
      (push (match-beginning 0) positions)
      (setq start (match-end 0)))
    (setq positions (nreverse positions))
    (cond
     ((null positions)
      (user-error "replace_region failed: old_text did not match"))
     ((and (null occurrence) (> (length positions) 1))
      (user-error "replace_region failed: old_text matched multiple times; specify occurrence"))
     (t
      (let* ((index (or occurrence 1)))
        (unless (and (integerp index) (> index 0))
          (user-error "replace_region occurrence must be a positive integer"))
        (let ((pos (nth (1- index) positions)))
        (unless pos
          (user-error "replace_region failed: occurrence %s not found" index))
        (concat (substring text 0 pos)
                new
                (substring text (+ pos (length old))))))))))

(defun my/gptel-tool-list-files (&rest args)
  "List files under :dir matching :glob. Returns relative paths."
  (setq args (my/gptel--args->plist args '("dir" "glob" "max")))
  (let* ((dir (file-name-as-directory
               (my/gptel--canonical-path (or (plist-get args :dir)
                                             (my/gptel--default-dir))
                                         t)))
         (glob (or (plist-get args :glob) "**/*"))
         (max (min 500 (or (plist-get args :max) 200))))
    (my/gptel--assert-allowed dir)
    (let* ((files (file-expand-wildcards (expand-file-name glob dir) t))
           (files (cl-remove-if #'file-directory-p files))
           (files (cl-subseq files 0 (min max (length files)))))
      (mapconcat (lambda (file) (file-relative-name file dir)) files "\n"))))

(defun my/gptel-tool-rg (&rest args)
  "Ripgrep search under :dir for :pattern; optional :glob; capped."
  (setq args (my/gptel--args->plist args '("dir" "pattern" "glob" "max")))
  (let* ((dir (file-name-as-directory
               (my/gptel--canonical-path (or (plist-get args :dir)
                                             (my/gptel--default-dir))
                                         t)))
         (pattern (or (plist-get args :pattern) (user-error "Missing :pattern")))
         (glob (plist-get args :glob))
         (max (min my/gptel-rg-max-hits (or (plist-get args :max) 120))))
    (my/gptel--assert-allowed dir)
    (unless (executable-find "rg")
      (user-error "ripgrep (rg) not found on PATH"))
    (let* ((default-directory dir)
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
  (setq args (my/gptel--args->plist args '("path" "start_line" "end_line")))
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (start (plist-get args :start_line))
         (end (plist-get args :end_line)))
    (my/gptel--read-lines path start end)))

(defun my/gptel-tool_head (&rest args)
  "Read first :n lines of :path, capped."
  (setq args (my/gptel--args->plist args '("path" "n")))
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (n (min my/gptel-max-lines (or (plist-get args :n) 60))))
    (my/gptel--read-lines path 1 n)))

(defun my/gptel-tool_tail (&rest args)
  "Read last :n lines of :path, capped (from first max-bytes chunk)."
  (setq args (my/gptel--args->plist args '("path" "n")))
  (let* ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
         (n (min my/gptel-max-lines (or (plist-get args :n) 60))))
    (my/gptel--assert-allowed path)
    (my/gptel--assert-text-file path)
    (with-temp-buffer
      (insert-file-contents path nil 0 my/gptel-max-bytes)
      (let* ((lines (split-string (buffer-string) "\n"))
             (take (last lines (min n (length lines)))))
        (mapconcat #'identity take "\n")))))

;;; Interactive convenience (for you, not for the model)

(defun my/gptel-add-extra-root (dir)
  "Add DIR to `my/gptel-extra-roots' persistently."
  (interactive "DAdd extra root directory: ")
  (setq dir (my/gptel--tru-dir dir))
  (add-to-list 'my/gptel-extra-roots dir)
  (customize-save-variable 'my/gptel-extra-roots my/gptel-extra-roots)
  (message "Added extra root (persistent): %s" dir))

(defun my/gptel-add-session-root (dir)
  "Add DIR to `my/gptel-session-roots' for the current Emacs session."
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
  "Show current allowed roots."
  (interactive)
  (message "Allowed roots: %s"
           (string-join (my/gptel--allowed-roots) ", ")))

(defun my/gptel-set-write-root (dir)
  "Set the current buffer's write root to DIR.
If DIR is inside a git repo, use the repo root as the write root."
  (interactive "DSet gptel write root: ")
  (let ((root (my/gptel--manual-write-root dir)))
    (my/gptel--set-write-root-internal root 'manual)
    (message "gptel write root -> %s" my/gptel-write-root)))

(defun my/gptel-set-write-root-to-project ()
  "Set the current buffer's write root to the current git repo root."
  (interactive)
  (let ((root (or (my/gptel--git-root default-directory)
                  (user-error "Current buffer is not inside a git repo; set a write root manually"))))
    (my/gptel--set-write-root-internal root 'manual)
    (message "gptel write root -> %s" my/gptel-write-root)))

(defun my/gptel-clear-write-root ()
  "Clear the current buffer's write root and edit-session state."
  (interactive)
  (setq-local my/gptel-write-root nil)
  (setq-local my/gptel-write-root-source nil)
  (setq-local my/gptel-edit-session-started nil)
  (message "Cleared gptel write root"))

(defun my/gptel-reset-edit-session ()
  "Reset the current buffer's write state."
  (interactive)
  (my/gptel-clear-write-root)
  (message "Reset gptel edit session"))

(defun my/gptel-show-write-root ()
  "Show the current write root and edit-session state."
  (interactive)
  (message "gptel write root: %s | session started: %s"
           (or my/gptel-write-root "(none)")
           (if my/gptel-edit-session-started "yes" "no")))

;;; Arg parsing helpers

(defun my/gptel--args->plist (args names)
  "If ARGS looks positional, convert it to a plist using NAMES order."
  (cond
   ((and (consp args) (keywordp (car args))) args)
   ((and (consp args) (consp (car args))) args)
   (t
    (let (plist)
      (cl-loop for name in names
               for val in args
               do (setq plist (plist-put plist (intern (concat ":" name)) val)))
      plist))))

;;; Mutating tool implementations

(defun my/gptel-tool-create-file (&rest args)
  "Create :path with :content."
  (setq args (my/gptel--args->plist args '("path" "content")))
  (let ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
        (content (or (plist-get args :content) "")))
    (my/gptel--assert-text-content content)
    (my/gptel--write-file-op
     path
     (lambda (target _state)
       (when (file-exists-p target)
         (user-error "create_file failed: file already exists: %s" target))
       (my/gptel--write-file-string target content)
       (format "Created %s" target)))))

(defun my/gptel-tool-write-file (&rest args)
  "Overwrite :path with :content."
  (setq args (my/gptel--args->plist args '("path" "content")))
  (let ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
        (content (or (plist-get args :content) "")))
    (my/gptel--assert-text-content content)
    (my/gptel--write-file-op
     path
     (lambda (target _state)
       (when (and (file-exists-p target) (not (file-directory-p target)))
         (my/gptel--assert-text-file target))
       (my/gptel--write-file-string target content)
       (format "Wrote %s" target)))))

(defun my/gptel-tool-replace-region (&rest args)
  "Replace :old_text with :new_text in :path."
  (setq args (my/gptel--args->plist args '("path" "old_text" "new_text" "occurrence")))
  (let ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path"))))
        (old-text (or (plist-get args :old_text) (user-error "Missing :old_text")))
        (new-text (or (plist-get args :new_text) ""))
        (occurrence (plist-get args :occurrence)))
    (when (string-empty-p old-text)
      (user-error "replace_region failed: old_text must not be empty"))
    (my/gptel--assert-text-content new-text)
    (my/gptel--write-file-op
     path
     (lambda (target _state)
       (unless (file-exists-p target)
         (user-error "replace_region failed: file does not exist: %s" target))
       (let* ((original (my/gptel--read-file-string target))
              (updated (my/gptel--replace-nth original old-text new-text occurrence)))
         (my/gptel--write-file-string target updated)
         (format "Updated %s" target))))))

(defun my/gptel-tool-delete-file (&rest args)
  "Delete :path."
  (setq args (my/gptel--args->plist args '("path")))
  (let ((path (expand-file-name (or (plist-get args :path) (user-error "Missing :path")))))
    (my/gptel--write-file-op
     path
     (lambda (target _state)
       (unless (file-exists-p target)
         (user-error "delete_file failed: file does not exist: %s" target))
       (when (file-directory-p target)
         (user-error "delete_file only supports files: %s" target))
       (delete-file target)
       (format "Deleted %s" target)))))

;;; Relevant buffer tools

(defvar my/gptel-relevant-buffers nil
  "List of buffer names explicitly marked as relevant for gptel buffer tools.")

(defun my/gptel--buffer-live-p (name)
  "Return non-nil if buffer NAME is live."
  (when-let ((buf (get-buffer name)))
    (buffer-live-p buf)))

(defun my/gptel--assert-relevant-buffer (name)
  "Require NAME to be a live relevant buffer."
  (unless (member name my/gptel-relevant-buffers)
    (user-error "Buffer not marked relevant: %s" name))
  (unless (my/gptel--buffer-live-p name)
    (user-error "Buffer not live: %s" name))
  name)

(defun my/gptel-tool-list-relevant-buffers (&rest _args)
  "List buffers marked relevant (one per line)."
  (setq my/gptel-relevant-buffers
        (cl-remove-if-not #'my/gptel--buffer-live-p my/gptel-relevant-buffers))
  (string-join my/gptel-relevant-buffers "\n"))

(defun my/gptel-tool-read-buffer-range (&rest args)
  "Read inclusive line range [start_line,end_line] from a relevant buffer."
  (setq args (my/gptel--args->plist args '("buffer" "start_line" "end_line")))
  (let* ((name (plist-get args :buffer))
         (start (max 1 (or (plist-get args :start_line) 1)))
         (end (max start (or (plist-get args :end_line) (+ start 60))))
         (want (min my/gptel-max-lines (1+ (- end start)))))
    (my/gptel--assert-relevant-buffer name)
    (with-current-buffer name
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- start))
        (let ((beg (point)))
          (forward-line want)
          (buffer-substring-no-properties beg (point)))))))

(defun my/gptel-tool-search-buffer (&rest args)
  "Search relevant buffers for PATTERN."
  (setq args (my/gptel--args->plist args '("buffer" "pattern" "context")))
  (let* ((pattern (or (plist-get args :pattern)
                      (user-error "Missing :pattern")))
         (context (max 0 (or (plist-get args :context) 0)))
         (buffers
          (if-let ((name (plist-get args :buffer)))
              (progn
                (my/gptel--assert-relevant-buffer name)
                (list name))
            (cl-remove-if-not #'my/gptel--buffer-live-p
                              my/gptel-relevant-buffers)))
         (results '())
         (lines-used 0))
    (dolist (name buffers)
      (with-current-buffer name
        (save-excursion
          (goto-char (point-min))
          (while (and (< lines-used my/gptel-max-lines)
                      (re-search-forward pattern nil t))
            (let* ((match-line (line-number-at-pos))
                   (start-line (max 1 (- match-line context)))
                   (end-line (+ match-line context)))
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- start-line))
                (let ((block-start (point)))
                  (forward-line (1+ (- end-line start-line)))
                  (let* ((text (buffer-substring-no-properties block-start (point)))
                         (header (format "Buffer: %s (match at line %d)\n"
                                         name match-line))
                         (chunk (concat header text "\n")))
                    (push chunk results)
                    (cl-incf lines-used (1+ (- end-line start-line)))))))))))
    (if results
        (mapconcat #'identity (nreverse results) "\n")
      "")))

;;; Registration

(defun my/gptel-register-tools ()
  "Register local tools with gptel."
  (when (fboundp 'gptel-make-tool)
    (gptel-make-tool
     :name "list_files"
     :function #'my/gptel-tool-list-files
     :description "List files under dir matching glob. Returns paths only."
     :args '((:name "dir" :type "string" :optional t)
             (:name "glob" :type "string" :optional t)
             (:name "max" :type "number" :optional t)))

    (gptel-make-tool
     :name "rg"
     :function #'my/gptel-tool-rg
     :description "Ripgrep search under dir for pattern; optional glob; capped."
     :args '((:name "dir" :type "string" :optional t)
             (:name "pattern" :type "string")
             (:name "glob" :type "string" :optional t)
             (:name "max" :type "number" :optional t)))

    (gptel-make-tool
     :name "read_range"
     :function #'my/gptel-tool-read-range
     :description "Read file line range [start_line,end_line] (inclusive), capped."
     :args '((:name "path" :type "string")
             (:name "start_line" :type "number" :optional t)
             (:name "end_line" :type "number" :optional t)))

    (gptel-make-tool
     :name "head"
     :function #'my/gptel-tool_head
     :description "Read first n lines of path, capped."
     :args '((:name "path" :type "string")
             (:name "n" :type "number" :optional t)))

    (gptel-make-tool
     :name "tail"
     :function #'my/gptel-tool_tail
     :description "Read last n lines of path, capped (from first max-bytes chunk)."
     :args '((:name "path" :type "string")
             (:name "n" :type "number" :optional t)))

    (gptel-make-tool
     :name "create_file"
     :function #'my/gptel-tool-create-file
     :description "Create a new text file at path with exact content."
     :args '((:name "path" :type "string")
             (:name "content" :type "string")))

    (gptel-make-tool
     :name "write_file"
     :function #'my/gptel-tool-write-file
     :description "Overwrite a file with exact content. Refuses existing binary files."
     :args '((:name "path" :type "string")
             (:name "content" :type "string")))

    (gptel-make-tool
     :name "replace_region"
     :function #'my/gptel-tool-replace-region
     :description "Replace exact old_text with new_text in a text file."
     :args '((:name "path" :type "string")
             (:name "old_text" :type "string")
             (:name "new_text" :type "string")
             (:name "occurrence" :type "number" :optional t)))

    (gptel-make-tool
     :name "delete_file"
     :function #'my/gptel-tool-delete-file
     :description "Delete a file at path."
     :args '((:name "path" :type "string")))

    (gptel-make-tool
     :name "list_relevant_buffers"
     :function #'my/gptel-tool-list-relevant-buffers
     :description "List buffers explicitly marked relevant by the user."
     :args '())

    (gptel-make-tool
     :name "read_buffer_range"
     :function #'my/gptel-tool-read-buffer-range
     :description "Read buffer line range [start_line,end_line] from a relevant buffer."
     :args '((:name "buffer" :type "string")
             (:name "start_line" :type "number" :optional t)
             (:name "end_line" :type "number" :optional t)))

    (gptel-make-tool
     :name "search_buffer"
     :function #'my/gptel-tool-search-buffer
     :description "Search relevant buffers for a regexp pattern."
     :args '((:name "pattern" :type "string")
             (:name "buffer" :type "string" :optional t)
             (:name "context" :type "number" :optional t)))))

(provide 'my-gptel-tools)
;;; my-gptel-tools.el ends here
