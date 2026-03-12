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
(declare-function my/gptel--project-root "my-gptel-tools" ())

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
  (let* ((roots (ignore-errors (my/gptel--allowed-roots)))
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
      (format "Allowed roots: %s"
              (if roots
                  (string-join roots ", ")
                "None"))
      (format "Current working project: %s" cwd)
      (format "Active write root: %s" write-root)
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
  (unless (ignore-errors (my/gptel--allowed-path-p path))
    (user-error "File not under allowed roots: %s (allowed roots: %s)"
                (my/gptel--canon-file path)
                (string-join (my/gptel--allowed-roots) ", ")))
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
                              (when (ignore-errors (my/gptel--allowed-path-p f))
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
           (if my/gptel-relevant-buffers
               (string-join my/gptel-relevant-buffers ", ")
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
            (while (and (org-up-heading-safe)
                        (> (org-current-level) 2)))
            (when (= (org-current-level) 2) t))
        (error nil)))))

(defun my/gptel-new-topic (title)
  "Create a new level-2 Topic heading named TITLE.

Works from anywhere in the file:
- If inside a level-2 topic subtree, inserts after that topic.
- Otherwise inserts at end of buffer (respecting narrowing if active)."
  (interactive "sTopic title: ")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))

  (let ((in-topic (my/gptel--goto-current-topic)))
    (cond
     (in-topic
      (org-end-of-subtree t t))
     (t
      ;; If narrowed, insert at end of visible region.
      (goto-char (point-max)))))

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
as usual."
  (interactive)
  (my/gptel--assert-topic)

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

(defun my/gptel-normalize-response-headings (beg end)
  "Normalize Org headings in the assistant response between BEG and END.

Narrow to the inserted response region from BEG to END, locate the
`@assistant' marker to find where assistant content begins, then scan
headings under that point.

The shallowest heading found is promoted so that its level becomes 4
(`\"****\"'), and all other headings are shifted by the same number of
stars, preserving their relative depth. Headings that are already at
level 4 or deeper are never demoted; if the minimum level is >= 4, the
buffer is left unchanged."
  (when (and (derived-mode-p 'org-mode)
             (integer-or-marker-p beg)
             (integer-or-marker-p end)
             (< beg end))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char (point-min))

        ;; locate assistant marker
        (when (re-search-forward "^@assistant\\(?::\\)?[ \t]*$" nil t)
          (forward-line 1)

          ;; pass 1: find shallowest heading
          (let ((content-start (point))
                min-depth)
            (save-excursion
              (goto-char content-start)
              (while (re-search-forward "^\\(\\*+\\)[ \t]" nil t)
                (let ((depth (length (match-string 1))))
                  (setq min-depth (if min-depth
                                      (min min-depth depth)
                                    depth)))))

            ;; pass 2: apply normalization
            (when min-depth
              (let ((delta (max 0 (- 4 min-depth))))
                (when (> delta 0)
                  (goto-char content-start)
                  (while (re-search-forward "^\\(\\*+\\)\\([ \t]\\)" nil t)
                    (replace-match
                     (concat (match-string 1) (make-string delta ?*) (match-string 2))
                     t t)))))))))))


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
