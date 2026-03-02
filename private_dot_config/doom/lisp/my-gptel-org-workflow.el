;;; lisp/my-gptel-org-workflow.el -*- lexical-binding: t; -*-
;;
;; Structured Org-mode workflow layer for gptel.
;;
;; This module enforces:
;;   **   = Topic (human-controlled)
;;   ***  = Conversation entries
;;   ****+ = Assistant structure
;;
;; It also installs:
;;   - A dynamic system message
;;   - Heading demotion safeguards
;;   - Automatic *** entry creation
;;
;; IMPORTANT:
;;   This module depends on functions from `my-gptel-tools.el`,
;;   specifically:
;;
;;     - `my/gptel--allowed-roots`
;;     - `my/gptel--project-root`
;;
;;   Therefore:
;;
;;     (require 'my-gptel-tools)
;;     (require 'my-gptel-org-workflow)
;;
;;   must be evaluated in that order.
;;
;;   In Doom, ensure this in config.el:
;;
;;     (add-load-path! "lisp")
;;     (require 'my-gptel-tools)
;;     (require 'my-gptel-org-workflow)
;;
;;   and call `my/gptel-register-tools` inside (after! gptel ...).
;;

(require 'org)
(require 'gptel)

(defgroup my/gptel-org-workflow nil
  "Structured Org workflow layer for gptel."
  :group 'gptel)

;;; ------------------------------------------------------------------
;;; System Message
;;; ------------------------------------------------------------------

(defun my/gptel-system-message ()
  "Dynamic system message for structured Org + Python workflow.

Relies on:
  - `my/gptel--allowed-roots`
  - `my/gptel--project-root`

These must be defined in `my-gptel-tools.el`."
  (let* ((roots (ignore-errors (my/gptel--allowed-roots)))
         (cwd   (or (ignore-errors (my/gptel--project-root))
                    default-directory)))
    (string-join
     (list
      "You are operating inside Emacs Org mode."
      ""
      "Org formatting rules:"
      "- Only create headings at level 4 or deeper."
      "- Never create level 2 or 3 headings."
      "- Use ~ for inline code; do not use = (verbatim)."
      "- Use #+begin_src blocks for multi-line code."
      "- Never emit Markdown."
      "- Do not escape the current subtree."
      ""
      "Shell command policy:"
      "- If suggesting a shell command, emit it inside:"
      "  #+begin_src sh"
      "  ..."
      "  #+end_src"
      "- Do not assume it will be run automatically."
      "- The user will decide whether to execute it."
      ""
      "Python block policy:"
      "- When emitting runnable Python, use:"
      "  #+begin_src python"
      "  ..."
      "  #+end_src"
      "- Code must be complete and executable."
      "- Respect the Python style requirements below."
      ""
      (format "Allowed roots: %s"
              (if roots
                  (string-join roots ", ")
                "None"))
      (format "Current working project: %s" cwd)
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
      "When files are mentioned, use available tools (list_files, rg, read_range, head, tail) rather than assuming contents."
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
  "Return PATH relative to the first matching allowed root, else relative to project, else absolute."
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
  (let ((tru (my/gptel--canon-file path)))
    (setq my/gptel-ref-history
          (cons tru (delete tru my/gptel-ref-history)))
    (when (> (length my/gptel-ref-history) my/gptel-ref-max-history)
      (setcdr (nthcdr (1- my/gptel-ref-max-history) my/gptel-ref-history) nil))))

(defun my/gptel-add-reference (&optional path)
  "Insert a lightweight reference to PATH (default: current buffer file).

This does NOT include file contents. It inserts an Org file link that the
assistant can use with tools (rg/read_range/head/tail) if needed.

With prefix arg (C-u), prompt for a file."
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
  (interactive)
  (let ((name (buffer-name (or buffer (current-buffer)))))
    (add-to-list 'my/gptel-relevant-buffers name)
    (message "Marked relevant: %s" name)))

(defun my/gptel-unmark-buffer-relevant (&optional buffer)
  (interactive)
  (let ((name (buffer-name (or buffer (current-buffer)))))
    (setq my/gptel-relevant-buffers
          (delete name my/gptel-relevant-buffers))
    (message "Unmarked relevant: %s" name)))

(defun my/gptel-clear-relevant-buffers ()
  (interactive)
  (setq my/gptel-relevant-buffers nil)
  (message "Cleared relevant buffers"))

(defun my/gptel-show-relevant-buffers ()
  (interactive)
  (message "Relevant buffers: %s"
           (if my/gptel-relevant-buffers
               (string-join my/gptel-relevant-buffers ", ")
             "(none)")))

(defun my/gptel-mark-project-buffers-relevant ()
  (interactive)
  (unless (fboundp 'projectile-project-buffers)
    (user-error "projectile not available"))
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

;; (defun my/gptel-new-topic (title)
;;   "Create a new level-2 Topic heading as a sibling of the current topic."
;;   (interactive "sTopic title: ")
;;   (unless (derived-mode-p 'org-mode)
;;     (user-error "Not in Org mode"))

;;   ;; Ensure we're at a heading
;;   (org-back-to-heading t)

;;   ;; Ensure we are inside a level-2 topic
;;   (unless (= (org-current-level) 2)
;;     (user-error "Point must be inside a level-2 Topic heading"))

;;   ;; Move to end of this topic subtree
;;   (org-end-of-subtree t t)

;;   ;; Insert new sibling topic
;;   (insert (format "\n** %s\n" title))
;;   (insert ":PROPERTIES:\n:GPTEL_TOPIC: t\n:END:\n\n")

;;   ;; Optional: first question entry
;;   (insert (format "*** %s — Question\n"
;;                   (format-time-string "%Y-%m-%d %H:%M")))
;;   (insert "@user:\n\n")
;;   (forward-line -1))

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
  "Create a new level-2 Topic heading after the current topic subtree.

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

Does NOT send. After writing your prompt under @user:,
use `gptel-send` (e.g. C-c RET) as usual."
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

(defun my/gptel-wrap-response (response info)
  "Wrap assistant RESPONSE inside a structured *** Response heading."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      ;; Move to end of the most recent *** entry
      (org-back-to-heading t)
      (let ((level (org-current-level)))
        (when (= level 3)
          ;; Move to end of that subtree
          (org-end-of-subtree t t)

          ;; Insert structured response heading
          (insert (format "\n*** %s — Response\n"
                          (format-time-string "%Y-%m-%d %H:%M")))
          (insert "@assistant:\n\n"))))))

;; (defun my/gptel-demote-headings (_response _info)
;;   "Ensure assistant headings are level 4 or deeper.

;; This prevents the assistant from escaping the subtree."
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (let ((min-level 4))
;;         (goto-char (point-min))
;;         (while (re-search-forward "^\\(\\*+\\) " nil t)
;;           (let ((n (length (match-string 1))))
;;             (when (< n min-level)
;;               (replace-match
;;                (concat (make-string min-level ?*) " ")
;;                nil nil))))))))

(defun my/gptel-normalize-response-headings (beg end)
  "Normalize assistant response headings relative to containing heading.

This operates only on the inserted response region from BEG to END."
  (when (derived-mode-p 'org-mode)
    (let (base-depth)
      (when (and (integer-or-marker-p beg)
                 (integer-or-marker-p end)
                 (< beg end))
        (save-excursion
          (setq base-depth
                (condition-case nil
                    (progn
                      (goto-char (max (point-min) (1- beg)))
                      (org-back-to-heading t)
                      (org-current-level))
                  (error nil))))
        (when base-depth
          (save-excursion
            (save-restriction
              (let ((beg-marker (copy-marker beg))
                    (end-marker (copy-marker end t)))
                (unwind-protect
                    (progn
                      (narrow-to-region beg-marker end-marker)
                      (goto-char (point-min))
                      ;; Repair malformed heading prefixes like "**/" -> "** ".
                      (while (re-search-forward "^\\(\\*+\\)/[ \t]*" nil t)
                        (replace-match (concat (match-string 1) " ") nil nil))

                      (let (headings
                            min-depth
                            target-min
                            delta
                            saw-heading)
                        (goto-char (point-min))
                        (while (re-search-forward "^\\(\\*+\\)[ \t]" nil t)
                          (let ((pos (match-beginning 0))
                                (depth (length (match-string 1))))
                            (setq saw-heading t)
                            (push (cons pos depth) headings)
                            (setq min-depth (if min-depth (min min-depth depth) depth))))

                        (when saw-heading
                          (setq target-min (max 4 (1+ base-depth))
                                delta (max 0 (- target-min min-depth)))

                          (setq headings (sort headings (lambda (a b) (> (car a) (car b)))))

                          ;; Apply replacements from bottom to top to avoid stale positions.
                          (dolist (h headings)
                            (goto-char (car h))
                            (when (looking-at "^\\(\\*+\\)\\([ \t]\\)")
                              (let* ((old-depth (cdr h))
                                     (new-depth (+ old-depth delta))
                                     (new-prefix (concat (make-string new-depth ?*) (match-string 2))))
                                (replace-match new-prefix nil nil))))

                          (goto-char (point-min))
                          (while (re-search-forward "^\\(\\*+\\)[ \t]" nil t)
                            (let ((depth (length (match-string 1))))
                              (when (< depth target-min)
                                (error "Heading depth invariant violated in response region (target-min=%d, found=%d, line=%d)"
                                       target-min
                                       depth
                                       (line-number-at-pos))))))))
                  (set-marker beg-marker nil)
                  (set-marker end-marker nil))))))))))

;;; ------------------------------------------------------------------
;;; Activation
;;; ------------------------------------------------------------------

(defun my/gptel-org-workflow-enable ()
  "Enable structured Org workflow for gptel."
  (interactive)

  ;; System message
  (setq gptel--system-message #'my/gptel-system-message)

  ;; Order matters:
  ;; 1. Wrap response
  (add-hook 'gptel-pre-response-functions
            #'my/gptel-wrap-response)

  ;; 2. Then normalize headings
  (add-hook 'gptel-post-response-functions
            #'my/gptel-normalize-response-headings))

(defun my/gptel-org-workflow-disable ()
  (interactive)

  (setq gptel--system-message nil)

  (remove-hook 'gptel-pre-response-functions
               #'my/gptel-wrap-response)

  (remove-hook 'gptel-post-response-functions
               #'my/gptel-normalize-response-headings))

(provide 'my-gptel-org-workflow)
;;; my-gptel-org-workflow.el ends here
