;;; my-gptel-org-workflow.el --- Structured Org workflow for gptel -*- lexical-binding: t; -*-

;;; Commentary:

;; Structured Org-mode workflow layer for gptel.
;;
;; This module enforces:
;; - `**' as the human-controlled topic level
;; - `***' as conversation entries
;; - `****+' as assistant structure
;;
;; It also installs a dynamic system message, heading normalization, and
;; commands for creating topics and conversation entries.

;;; Code:

(require 'org)
(require 'gptel)

(declare-function projectile-project-buffers "ext:projectile")
(declare-function my/gptel--allowed-path-p "my-gptel-tools" (path))
(declare-function my/gptel--allowed-roots "my-gptel-tools" ())
(declare-function my/gptel--assert-readable-path "my-gptel-tools" (path))
(declare-function my/gptel--delete-dups-stable "my-gptel-tools" (items))
(declare-function my/gptel--ignore-globs "my-gptel-tools" ())
(declare-function my/gptel--normalize-root-entry "my-gptel-tools" (path))
(declare-function my/gptel--project-root "my-gptel-tools" ())
(declare-function my/gptel--session-scope "my-gptel-tools" ())
(declare-function my/gptel--split-property-entries "my-gptel-tools" (value))
(declare-function my/gptel--relevant-buffers "my-gptel-tools" ())

(defvar my/gptel-relevant-buffers nil
  "List of buffers marked relevant for gptel buffer tools.")
(defvar my/gptel-write-root nil
  "Active write root for the current gptel conversation buffer.")

(defgroup my/gptel-org-workflow nil
  "Structured Org workflow layer for gptel."
  :group 'gptel)

;;; ------------------------------------------------------------------
;;; System Message
;;; ------------------------------------------------------------------

(defun my/gptel-system-message ()
  "Dynamic system message for structured Org + Python workflow.

Relies on:
  - `my/gptel--allowed-roots'
  - `my/gptel--project-root'
  - `my/gptel-write-root'

These must be defined in `my-gptel-tools.el`."
  (let* ((scope (ignore-errors (my/gptel--session-scope)))
         (roots (plist-get scope :allowed-roots))
         (relevant-buffers (plist-get scope :relevant-buffers))
         (ignore-globs (plist-get scope :ignore-globs))
         (scoped-p (plist-get scope :scoped-p))
         (inherit-default-roots (plist-get scope :inherit-default-roots))
         (topic-heading (plist-get scope :topic-heading))
         (topic-id (plist-get scope :topic-id))
         (cwd   (or (ignore-errors (my/gptel--project-root))
                    default-directory))
         (write-root (or (and (boundp 'my/gptel-write-root) my/gptel-write-root)
                         "(not set)")))
    (string-join
     (list
      "Output strictly valid GitHub-Flavored Markdown."
      ""
      "Markdown output rules:"
      "- Use # headings only."
      "- Never emit org-mode syntax."
      "- Never emit level-1 headings (#)."
      "- Never emit level-2 headings (##)."
      "- Always leave a blank line after headings."
      "- Use fenced code blocks with language tags."
      ""
      "Shell command policy:"
      "- If suggesting a shell command, emit it inside:"
      "  ```sh"
      "  ..."
      "  ```"
      "- Do not assume it will be run automatically."
      "- The user will decide whether to execute it."
      ""
      "Python block policy:"
      "- When emitting runnable Python, use:"
      "  ```python"
      "  ..."
      "  ```"
      "- Code must be complete and executable."
      "- Respect the Python style requirements below."
      ""
      (format "Session scope: %s" (if scoped-p "active" "default"))
      (format "Session topic: %s"
              (or (and topic-heading topic-id
                       (format "%s [GPTEL_TOPIC=%s]" topic-heading topic-id))
                  topic-heading
                  topic-id
                  "(none)"))
      (format "Inherit default roots: %s"
              (if inherit-default-roots "yes" "no"))
      (format "Allowed roots: %s"
              (if roots
                  (string-join roots ", ")
                "None"))
      (format "Relevant buffers: %s"
              (if relevant-buffers
                  (string-join relevant-buffers ", ")
                "(none)"))
      (format "Ignore globs: %s"
              (if ignore-globs
                  (string-join ignore-globs ", ")
                "(none)"))
      (format "Current working project: %s" cwd)
      (format "Active write root: %s" write-root)
      "Treat ignored-glob paths as off-limits even if they are mentioned elsewhere."
      "Mutating tools are only available in the editing preset."
      "When mutating tools are enabled, writes require an explicit write root,"
      "a GPTEL_WRITE_ROOT Org property, or a first-target git repo inference."
      ""
      "Python style requirements:"
      "- Python 3.11+."
      "- Mandatory type hints."
      "- Google-style docstrings."
      "- Idiomatic xarray."
      "- Minimal abstraction."
      "- Max line length 110."
      "- PEP8 naming conventions."
      "- Prefer pathlib."
      "- Avoid overengineering; prioritise scientific readability."
      ""
      "Relevant buffers are explicitly marked by the user."
      "Use tools: list_relevant_buffers, search_buffer, read_buffer_range."
      "Prefer search_buffer to reading large ranges."
      ""
      (concat "When files are mentioned, use available tools "
              "(list_files, rg, read_range, head, tail) rather than "
              "assuming contents.")
      ""
      "Reference policy:"
      "- Lines starting with \"Relevant file:\" and Org links like [[file:...]] are pointers."
      "- Use tools (rg/read_range/head/tail) to inspect them; do not assume contents."
      ""
      "If the file is currently open, prefer buffer tools over filesystem tools."
      )
     "\n")))

;;; ------------------------------------------------------------------
;;; Lightweight file references (gptel-add light)
;;; ------------------------------------------------------------------

(defcustom my/gptel-ref-max-history 50
  "Max number of file references to remember."
  :type 'integer
  :group 'my/gptel-org-workflow)

(defvar my/gptel-ref-history nil
  "Recently referenced files for gptel (absolute paths, canonicalized).")

(defconst my/gptel-session-scope-properties
  '("GPTEL_ALLOWED_ROOTS"
    "GPTEL_RELEVANT_BUFFERS"
    "GPTEL_IGNORE_GLOBS"
    "GPTEL_INHERIT_DEFAULT_ROOTS")
  "Org properties used for gptel session scoping.")

(defun my/gptel--canon-file (path)
  "Canonicalize PATH for stable de-duplication."
  (file-truename (expand-file-name path)))

(defun my/gptel--best-relative-path (path)
  "Return PATH relative to the best available root.
Prefer the first matching allowed root, else the project root, else PATH."
  (let* ((tru (my/gptel--canon-file path))
         (roots (ignore-errors (my/gptel--allowed-roots))))
    (or
     (when roots
       (catch 'done
         (dolist (r roots)
           (when (string-prefix-p r tru)
             (throw 'done (file-relative-name tru r))))))
     (when-let ((pr (ignore-errors (my/gptel--project-root))))
       (file-relative-name tru (file-name-as-directory (file-truename pr))))
     tru)))

(defun my/gptel--remember-ref (path)
  "Remember PATH in the recent gptel reference history."
  (let ((tru (my/gptel--canon-file path)))
    (setq my/gptel-ref-history
          (cons tru (delete tru my/gptel-ref-history)))
    (when (> (length my/gptel-ref-history) my/gptel-ref-max-history)
      (setcdr (nthcdr (1- my/gptel-ref-max-history) my/gptel-ref-history) nil))))

(defun my/gptel-add-reference (&optional path)
  "Insert a lightweight reference to PATH (default: current buffer file).

This does NOT include file contents. It inserts an Org file link that the
assistant can use with tools (rg/read_range/head/tail) if needed.

With a prefix argument, prompt for a file."
  (interactive
   (list (if current-prefix-arg
             (read-file-name "Reference file: " nil nil t)
           (buffer-file-name (buffer-base-buffer)))))
  (unless path
    (user-error "Current buffer is not visiting a file"))
  (condition-case err
      (my/gptel--assert-readable-path path)
    (user-error
     (user-error "%s" (cadr err))))
  (my/gptel--remember-ref path)
  (let ((rel (my/gptel--best-relative-path path)))
    ;; Avoid list bullets; keep it as plain lines.
    (insert (format "Relevant file: [[file:%s]]\n" rel))))

(defun my/gptel-add-reference-from-history ()
  "Pick a previously referenced file and insert it again."
  (interactive)
  (unless my/gptel-ref-history
    (user-error "No reference history yet"))
  (let* ((choice (completing-read "Reference: " my/gptel-ref-history nil t))
         (path (my/gptel--canon-file choice)))
    (my/gptel-add-reference path)))

(defun my/gptel-add-reference-from-open-buffers ()
  "Pick a file-visiting buffer under allowed roots and insert a reference."
  (interactive)
  (let* ((files
          (delq nil
                (mapcar (lambda (b)
                          (with-current-buffer b
                            (when-let ((f (buffer-file-name (buffer-base-buffer))))
                              (when (ignore-errors (my/gptel--assert-readable-path f))
                                (my/gptel--canon-file f)))))
                        (buffer-list))))
         (files (delete-dups files)))
    (unless files
      (user-error "No open file buffers under allowed roots"))
    (my/gptel-add-reference (completing-read "Open file: " files nil t))))

(defun my/gptel-insert-reference-block ()
  "Insert a compact block of recent references (for the current prompt)."
  (interactive)
  (unless my/gptel-ref-history
    (user-error "No reference history yet"))
  (insert "Relevant files (inspect via tools as needed):\n")
  (dolist (f my/gptel-ref-history)
    (insert (format "Relevant file: [[file:%s]]\n" (my/gptel--best-relative-path f)))))

;;; ------------------------------------------------------------------
;;; Buffer references
;;; ------------------------------------------------------------------

(defun my/gptel-mark-buffer-relevant (&optional buffer)
  "Mark BUFFER as relevant for gptel buffer tools."
  (interactive)
  (let ((name (buffer-name (or buffer (current-buffer)))))
    (add-to-list 'my/gptel-relevant-buffers name)
    (message "Marked relevant: %s" name)))

(defun my/gptel-unmark-buffer-relevant (&optional buffer)
  "Remove BUFFER from the relevant gptel buffer set."
  (interactive)
  (let ((name (buffer-name (or buffer (current-buffer)))))
    (setq my/gptel-relevant-buffers
          (delete name my/gptel-relevant-buffers))
    (message "Unmarked relevant: %s" name)))

(defun my/gptel-clear-relevant-buffers ()
  "Clear all buffers marked relevant for gptel."
  (interactive)
  (setq my/gptel-relevant-buffers nil)
  (message "Cleared relevant buffers"))

(defun my/gptel-show-relevant-buffers ()
  "Show buffers currently marked relevant for gptel."
  (interactive)
  (message "Relevant buffers: %s"
           (if-let ((buffers (ignore-errors (my/gptel--relevant-buffers))))
               (if buffers
                   (string-join buffers ", ")
                 "(none)")
             "(none)")))

(defun my/gptel-mark-project-buffers-relevant ()
  "Mark all current project buffers as relevant for gptel."
  (interactive)
  (unless (fboundp 'projectile-project-buffers)
    (user-error "Projectile not available"))
  (let ((n 0))
    (dolist (b (projectile-project-buffers))
      (with-current-buffer b
        (when (buffer-file-name)
          (my/gptel-mark-buffer-relevant b)
          (cl-incf n))))
    (message "Marked %d project buffers relevant" n)))

(defmacro my/gptel--with-current-topic (&rest body)
  "Run BODY from the current level-2 topic heading."
  (declare (indent 0) (debug t))
  `(progn
     (unless (derived-mode-p 'org-mode)
       (user-error "Not in Org mode"))
     (unless (my/gptel--goto-current-topic)
       (user-error "Not inside a gptel topic subtree"))
     ,@body))

(defun my/gptel--topic-property-value (property)
  "Return PROPERTY from the current gptel topic."
  (my/gptel--with-current-topic
    (org-entry-get nil property)))

(defun my/gptel--set-topic-property-value (property value)
  "Set PROPERTY to VALUE on the current gptel topic."
  (my/gptel--with-current-topic
    (org-set-property property value)))

(defun my/gptel--delete-topic-property (property)
  "Delete PROPERTY from the current gptel topic."
  (my/gptel--with-current-topic
    (org-delete-property property)))

(defun my/gptel--session-property-entries (property)
  "Return whitespace-separated PROPERTY entries from the current topic."
  (my/gptel--split-property-entries (or (my/gptel--topic-property-value property) "")))

(defun my/gptel--format-session-property-entries (entries)
  "Format session scope property ENTRIES as whitespace-separated text."
  (string-join
   (mapcar (lambda (entry)
             (if (string-match-p "[[:space:]\"'\\\\]" entry)
                 (prin1-to-string entry)
               entry))
           (my/gptel--delete-dups-stable (delq nil entries)))
   " "))

(defun my/gptel--buffer-scope-entry (&optional buffer)
  "Return a whitespace-safe scope entry for BUFFER."
  (let* ((buffer (or buffer (current-buffer)))
         (name (buffer-name buffer)))
    name))

(defun my/gptel--live-buffer-scope-candidates ()
  "Return live buffer names for scope completion."
  (my/gptel--delete-dups-stable
   (delq nil
         (mapcar (lambda (buffer)
                   (buffer-name buffer))
                 (buffer-list)))))

(defun my/gptel--read-root-list (prompt)
  "Read one or more directories using PROMPT."
  (let (roots done)
    (while (not done)
      (push (read-directory-name prompt nil nil t) roots)
      (setq done (not (y-or-n-p "Add another root? "))))
    (nreverse roots)))

(defun my/gptel--normalize-session-roots (roots)
  "Normalize ROOTS for storage in `GPTEL_ALLOWED_ROOTS'."
  (mapcar #'my/gptel--normalize-root-entry
          (if (listp roots) roots (list roots))))

(defun my/gptel-set-session-allowed-roots (roots)
  "Set `GPTEL_ALLOWED_ROOTS' on the current gptel topic to ROOTS."
  (interactive (list (my/gptel--read-root-list "Allowed root: ")))
  (my/gptel--set-topic-property-value
   "GPTEL_ALLOWED_ROOTS"
   (my/gptel--format-session-property-entries
    (my/gptel--normalize-session-roots roots)))
  (my/gptel-describe-session-scope))

(defun my/gptel-add-session-allowed-roots (roots)
  "Append ROOTS to `GPTEL_ALLOWED_ROOTS' on the current gptel topic."
  (interactive (list (my/gptel--read-root-list "Add allowed root: ")))
  (let ((entries (my/gptel--normalize-session-roots
                  (append (my/gptel--session-property-entries "GPTEL_ALLOWED_ROOTS")
                          roots))))
    (my/gptel--set-topic-property-value
     "GPTEL_ALLOWED_ROOTS"
     (my/gptel--format-session-property-entries entries)))
  (my/gptel-describe-session-scope))

(defun my/gptel--attachment-dir (&optional create)
  "Return the current Org attachment directory.
When CREATE is non-nil, create the attachment directory if needed.  Without
CREATE, only an existing attachment directory is accepted."
  (unless (derived-mode-p 'org-mode)
    (user-error "Attachment roots require Org mode"))
  (require 'org-attach)
  (save-excursion
    (unless (org-before-first-heading-p)
      (org-back-to-heading t))
    (let ((dir (org-attach-dir create nil)))
      (unless dir
        (user-error "No Org attachment directory for this heading"))
      (cond
       ((file-directory-p dir)
        (file-name-as-directory (file-truename dir)))
       (create
        (make-directory dir t)
        (file-name-as-directory (file-truename dir)))
       (t
        (user-error "Attachment directory does not exist; use a prefix arg to create it: %s" dir))))))

(defun my/gptel-add-attachment-dir-to-session-allowed-roots (&optional create)
  "Append the current heading's Org attachment dir to `GPTEL_ALLOWED_ROOTS'.
With prefix argument CREATE, create the attachment directory if needed."
  (interactive "P")
  (my/gptel-add-session-allowed-roots
   (list (my/gptel--attachment-dir create))))

(defun my/gptel-toggle-session-inherit-default-roots ()
  "Toggle `GPTEL_INHERIT_DEFAULT_ROOTS' on the current gptel topic."
  (interactive)
  (let* ((scope (my/gptel--session-scope))
         (inherit-default-roots (plist-get scope :inherit-default-roots))
         (next-value (if inherit-default-roots "nil" "yes")))
    (my/gptel--set-topic-property-value "GPTEL_INHERIT_DEFAULT_ROOTS" next-value)
    (message "GPTEL_INHERIT_DEFAULT_ROOTS -> %s" next-value)))

(defun my/gptel-add-session-relevant-buffers (buffers)
  "Append BUFFERS to `GPTEL_RELEVANT_BUFFERS' on the current gptel topic."
  (interactive
   (list (completing-read-multiple
          "Relevant buffers: "
          (my/gptel--live-buffer-scope-candidates)
          nil t)))
  (let ((entries (append (my/gptel--session-property-entries "GPTEL_RELEVANT_BUFFERS")
                         buffers)))
    (my/gptel--set-topic-property-value
     "GPTEL_RELEVANT_BUFFERS"
     (my/gptel--format-session-property-entries entries)))
  (my/gptel-describe-session-scope))

(defun my/gptel-set-session-relevant-buffers (buffers)
  "Replace `GPTEL_RELEVANT_BUFFERS' on the current gptel topic with BUFFERS."
  (interactive
   (list (completing-read-multiple
          "Relevant buffers: "
          (my/gptel--live-buffer-scope-candidates)
          nil t)))
  (if buffers
      (my/gptel--set-topic-property-value
       "GPTEL_RELEVANT_BUFFERS"
       (my/gptel--format-session-property-entries buffers))
    (my/gptel--delete-topic-property "GPTEL_RELEVANT_BUFFERS"))
  (my/gptel-describe-session-scope))

(defun my/gptel-add-current-buffer-to-session-scope (&optional buffer)
  "Append BUFFER to `GPTEL_RELEVANT_BUFFERS' on the current topic.

When called interactively from an Org topic, default to the most recent other
buffer, since the current buffer is often the chat buffer itself."
  (interactive
   (list (let ((default-buffer (if (derived-mode-p 'org-mode)
                                   (other-buffer (current-buffer) t)
                                 (current-buffer))))
           (get-buffer
            (completing-read "Buffer to add: "
                             (my/gptel--live-buffer-scope-candidates)
                             nil t (buffer-name default-buffer))))))
  (my/gptel-add-session-relevant-buffers
   (list (my/gptel--buffer-scope-entry (or buffer (current-buffer))))))

(defun my/gptel-add-session-ignore-globs (globs)
  "Append GLOBS to `GPTEL_IGNORE_GLOBS' on the current gptel topic."
  (interactive
   (list (my/gptel--split-property-entries
          (read-string "Ignore globs (space-separated): "))))
  (let ((entries (append (my/gptel--session-property-entries "GPTEL_IGNORE_GLOBS")
                         globs)))
    (my/gptel--set-topic-property-value
     "GPTEL_IGNORE_GLOBS"
     (my/gptel--format-session-property-entries entries)))
  (my/gptel-describe-session-scope))

(defun my/gptel-clear-session-scope-properties ()
  "Clear all gptel session-scope properties from the current topic."
  (interactive)
  (dolist (property my/gptel-session-scope-properties)
    (my/gptel--delete-topic-property property))
  (message "Cleared gptel session scope properties"))

;;; ------------------------------------------------------------------
;;; Topic Enforcement
;;; ------------------------------------------------------------------

(defun my/gptel--assert-topic ()
  "Ensure point is inside a level-2 Org topic."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (unless (org-current-level)
    (user-error "Not inside a heading"))
  (unless (= (org-current-level) 2)
    (user-error "Point must be inside a level-2 Topic heading")))

;;; ------------------------------------------------------------------
;;; Create New Topic Header
;;; ------------------------------------------------------------------

(defun my/gptel--goto-current-topic ()
  "Move point to the current level-2 topic heading, if any.
Returns non-nil if found."
  (when (derived-mode-p 'org-mode)
    (save-restriction
      ;; Respect narrowing, but still operate sanely within it.
      (widen)
      (when (org-before-first-heading-p)
        (goto-char (point-min)))
      (condition-case nil
          (progn
            ;; If inside a heading/subtree, climb up to level 2.
            (org-back-to-heading t)
            (while (> (org-current-level) 2)
              (org-up-heading-safe))
            (when (= (org-current-level) 2) t))
        (error nil)))))

(defun my/gptel--goto-current-section ()
  "Move point to the current level-1 section heading, if any.
Returns non-nil if found."
  (when (derived-mode-p 'org-mode)
    (save-restriction
      (widen)
      (condition-case nil
          (progn
            (unless (org-before-first-heading-p)
              (org-back-to-heading t))
            (while (> (org-current-level) 1)
              (org-up-heading-safe))
            (when (= (org-current-level) 1) t))
        (error nil)))))

(defun my/gptel-new-topic (title)
  "Create a new level-2 Topic heading named TITLE.

Works from anywhere in the file:
- If inside a level-2 topic subtree, inserts after that topic.
- Otherwise, if inside a level-1 section, inserts at the end of that section.
- If there is no current level-1 section, inserts at end of buffer."
  (interactive "sTopic title: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))

  (cond
   ((my/gptel--goto-current-topic)
    (org-end-of-subtree t t))
   ((my/gptel--goto-current-section)
    (org-end-of-subtree t t))
   (t
    ;; If narrowed, insert at end of visible region.
    (goto-char (point-max))))

  (unless (bolp) (insert "\n"))
  (insert (format "\n** %s\n" title))
  (insert ":PROPERTIES:\n:GPTEL_TOPIC: t\n:END:\n\n")
  (insert (format "*** %s — Question\n"
                  (format-time-string "%Y-%m-%d %H:%M")))
  (insert "@user\n\n")
  (forward-line -1))

;;; ------------------------------------------------------------------
;;; Create New Conversation Entry
;;; ------------------------------------------------------------------

(defun my/gptel-new-entry ()
  "Insert a new *** conversation entry under the current level-2 Topic.

Does not send. After writing your prompt under @user:, use `gptel-send'
as usual.  The command works from anywhere inside the topic subtree, including
previous prompt or response headings."
  (interactive)
  (unless (my/gptel--goto-current-topic)
    (user-error "Not inside a gptel topic subtree"))

  ;; Move to end of topic subtree
  (org-end-of-subtree t t)

  ;; Insert new structured entry
  (insert (format "\n*** %s — Question\n"
                  (format-time-string "%Y-%m-%d %H:%M")))
  (insert "@user\n\n")

  ;; Place point ready for writing
  (forward-line -1))

;;; ------------------------------------------------------------------
;;; Response processing
;;; ------------------------------------------------------------------

(defun my/gptel-ensure-conversation-node ()
  "Ensure point is inside a level-3 conversation node."
  (when (derived-mode-p 'org-mode)
    (let ((level (org-current-level)))
      (cond
       ;; already in conversation node
       ((= level 3))

       ;; inside topic → create conversation node
       ((= level 2)
        (org-end-of-subtree t t)
        (insert "\n*** Conversation\n"))

       ;; deeper inside previous response → escape and create new node
       ((> level 3)
        (org-back-to-heading t)
        (while (> (org-current-level) 2)
          (org-up-heading-safe))
        (org-end-of-subtree t t)
        (insert "\n*** Conversation\n"))))))

(defun my/gptel--org-block-boundary (line)
  "Return block boundary type for LINE, or nil.
The return value is `begin' or `end'."
  (let ((case-fold-search t))
    (cond
     ((string-match-p "^[ 	]*#\\+begin_" line) 'begin)
     ((string-match-p "^[ 	]*#\\+end_" line) 'end)
     (t nil))))

(defun my/gptel--each-content-line (beg end fn)
  "Call FN for each line between BEG and END.
FN receives LINE-BEG, LINE-END, and ACTIVE-P.  ACTIVE-P is nil inside Org
source/example/export blocks."
  (save-excursion
    (let ((limit (copy-marker end))
          (in-block nil))
      (goto-char beg)
      (while (< (point) limit)
        (let* ((line-beg (point))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-beg line-end))
               (boundary (my/gptel--org-block-boundary line)))
          (cond
           ((eq boundary 'begin)
            (funcall fn line-beg line-end nil)
            (setq in-block t))
           ((eq boundary 'end)
            (funcall fn line-beg line-end nil)
            (setq in-block nil))
           (t
            (funcall fn line-beg line-end (not in-block))))
          (forward-line 1)))
      (set-marker limit nil))))

(defun my/gptel--repair-malformed-response-headings (beg end)
  "Repair malformed assistant heading lines between BEG and END.
This handles lines like `**/ Heading', which can result from Markdown emphasis
conversion around model-written pseudo-headings."
  (my/gptel--each-content-line
   beg end
   (lambda (line-beg _line-end active-p)
     (when active-p
       (save-excursion
         (goto-char line-beg)
         (when (looking-at "^\\(\\*+\\)/[ 	]*\\(.+\\)$")
           (replace-match "\\1 \\2" t nil)))))))

(defun my/gptel--shallowest-response-heading (beg end)
  "Return the shallowest Org heading depth between BEG and END."
  (let (min-depth)
    (my/gptel--each-content-line
     beg end
     (lambda (line-beg _line-end active-p)
       (when active-p
         (save-excursion
           (goto-char line-beg)
           (when (looking-at "^\\(\\*+\\)[ 	]")
             (let ((depth (length (match-string 1))))
               (setq min-depth (if min-depth (min min-depth depth) depth))))))))
    min-depth))

(defun my/gptel--shift-response-headings (beg end delta)
  "Shift Org headings between BEG and END by DELTA stars."
  (when (> delta 0)
    (my/gptel--each-content-line
     beg end
     (lambda (line-beg _line-end active-p)
       (when active-p
         (save-excursion
           (goto-char line-beg)
           (when (looking-at "^\\(\\*+\\)\\([ 	]\\)")
             (replace-match
              (concat (match-string 1) (make-string delta ?*) (match-string 2))
              t t))))))))

(defun my/gptel--cleanup-assistant-content (beg end)
  "Repair and normalize assistant content between BEG and END."
  (let ((end-marker (copy-marker end)))
    (unwind-protect
        (progn
          (my/gptel--repair-malformed-response-headings beg end-marker)
          (when-let ((min-depth (my/gptel--shallowest-response-heading beg end-marker)))
            (my/gptel--shift-response-headings beg end-marker (max 0 (- 4 min-depth)))))
      (set-marker end-marker nil))))

(defun my/gptel-normalize-response-headings (beg end)
  "Normalize Org headings in the assistant response between BEG and END.

Narrow to the inserted response region from BEG to END, locate the
`@assistant' marker to find where assistant content begins, then repair malformed
heading lines and scan headings under that point.

The shallowest heading found is promoted to level 4 (four stars),
and all other headings are shifted by the same number of stars,
preserving their relative depth.  Headings that are already at level 4 or deeper
are never demoted; if the minimum level is >= 4, the buffer is left unchanged.
Org source/example/export blocks inside the response are ignored."
  (when (and (derived-mode-p 'org-mode)
             (integer-or-marker-p beg)
             (integer-or-marker-p end)
             (< beg end))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (when (re-search-forward "^@assistant\\(?::\\)?[ 	]*$" nil t)
          (forward-line 1)
          (my/gptel--cleanup-assistant-content (point) (point-max)))))))

(defun my/gptel--cleanup-response-region (beg end)
  "Cleanup assistant responses between BEG and END.
Return the number of assistant markers processed."
  (let ((count 0))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))
        (while (re-search-forward "^@assistant\\(?::\\)?[ 	]*$" nil t)
          (let ((content-start (progn (forward-line 1) (point)))
                (content-end (save-excursion
                               (or (and (re-search-forward
                                         "^\\(\\*\\*\\* .*\\(?:Question\\|Prompt\\|Response\\|Conversation\\)\\|@user\\(?::\\)?[ 	]*$\\)"
                                         nil t)
                                        (line-beginning-position))
                                   (point-max)))))
            (my/gptel--cleanup-assistant-content content-start content-end)
            (setq count (1+ count))
            (goto-char content-end)))))
    count))

(defun my/gptel-cleanup-response-headings (beg end)
  "Explicitly repair and normalize saved gptel Org response headings.

When the region is active, clean only the selected region.  Otherwise clean the
current Org subtree.  This command is intentionally explicit; response cleanup is
not applied to a whole Org file automatically."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (unless (derived-mode-p 'org-mode)
       (user-error "Response cleanup requires Org mode or an active region"))
     (save-excursion
       (org-back-to-heading t)
       (list (point) (save-excursion (org-end-of-subtree t t) (point))))))
  (let ((count (my/gptel--cleanup-response-region beg end)))
    (message "Cleaned %d assistant response%s" count (if (= count 1) "" "s"))))

;;; ------------------------------------------------------------------
;;; Activation
;;; ------------------------------------------------------------------

(defun my/gptel-org-workflow-enable ()
  "Enable structured Org workflow for gptel."
  (interactive)

  ;; System message
  (setq gptel--system-message #'my/gptel-system-message)

  (add-hook 'gptel-post-response-functions
            #'my/gptel-normalize-response-headings))

(defun my/gptel-org-workflow-disable ()
  "Disable the structured Org workflow customizations for gptel."
  (interactive)

  (setq gptel--system-message nil)

  (remove-hook 'gptel-post-response-functions
               #'my/gptel-normalize-response-headings))

(provide 'my-gptel-org-workflow)
;;; my-gptel-org-workflow.el ends here
