;;; bm-md2org.el --- Convert Markdown buffers/regions to Org with pandoc -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Brendan Murphy
;;
;; Author: Brendan Murphy <hello@brendanmurphy.org>
;; Maintainer: Brendan Murphy <hello@brendanmurphy.org>
;; Created: May 28, 2026
;; Modified: May 28, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/brendan-m-murphy/bm-md2org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Small helpers for converting Markdown to Org using pandoc.
;;
;; Features:
;; - Convert current .md/.markdown buffer to a sibling .org file.
;; - Convert active region to Org and show the result in a popup buffer.
;; - Preserve special marked regions as Org example blocks.
;; - Optionally turn a single Markdown H1 into #+TITLE and promote headings.
;;
;; Required external command:
;;   pandoc

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup bm-md2org nil
  "Markdown to Org conversion helpers."
  :group 'text)

(defcustom bm-md2org-pandoc-command "pandoc"
  "Pandoc executable used by `bm-md2org'."
  :type 'string)

(defcustom bm-md2org-pandoc-from
  "markdown+lists_without_preceding_blankline-auto_identifiers"
  "Pandoc input format used for Markdown."
  :type 'string)

(defcustom bm-md2org-pandoc-to "org"
  "Pandoc output format."
  :type 'string)

(defcustom bm-md2org-single-h1-to-title t
  "When non-nil, convert a single Markdown H1 to #+TITLE.

This is applied before pandoc runs.  If the Markdown input contains exactly
one ATX level-1 heading of the form:

  # Title

then that heading is removed, the title is emitted as:

  #+TITLE: Title

and remaining Org headings are promoted by one level after conversion.

For example:

  # Title
  ## Section

becomes:

  #+TITLE: Title
  * Section"
  :type 'boolean)

(defcustom bm-md2org-example-marker-regexp
  "^\\s-*<!--\\s-*\\([^[:space:]>]+\\)\\s-*-->\\s-*$"
  "Regexp matching special HTML comment markers.

The first capture group should be marker text such as:

  gh-issues:start
  gh-issues:end

Only matching NAME:start / NAME:end pairs receive a #+name line.
Other start/end-like pairs are still converted to example blocks, but without
a name."
  :type 'regexp)

(defun bm-md2org--pandoc-args ()
  "Return pandoc arguments for Markdown to Org conversion."
  (list "-f" bm-md2org-pandoc-from
        "-t" bm-md2org-pandoc-to
        "--wrap=preserve"))

(defun bm-md2org--pandoc-string (text)
  "Convert Markdown TEXT to Org using pandoc."
  (unless (executable-find bm-md2org-pandoc-command)
    (user-error "Could not find pandoc executable: %s" bm-md2org-pandoc-command))
  (if (string-empty-p text)
      ""
    (let ((input-file (make-temp-file "bm-md2org-input-" nil ".md"))
          (output-file (make-temp-file "bm-md2org-output-" nil ".org"))
          (stderr-file (make-temp-file "bm-md2org-stderr-")))
      (unwind-protect
          (progn
            (with-temp-file input-file
              (insert text))
            (let ((exit-code
                   (apply #'call-process
                          bm-md2org-pandoc-command
                          nil
                          (list nil stderr-file)
                          nil
                          (append
                           (bm-md2org--pandoc-args)
                           (list "-o" output-file input-file)))))
              (unless (zerop exit-code)
                (let ((stderr
                       (if (file-exists-p stderr-file)
                           (string-trim
                            (with-temp-buffer
                              (insert-file-contents stderr-file)
                              (buffer-string)))
                         "")))
                  (user-error "pandoc failed with exit code %s%s%s"
                              exit-code
                              (if (string-empty-p stderr) "" ": ")
                              stderr)))
              (with-temp-buffer
                (insert-file-contents output-file)
                (buffer-string))))
        (dolist (file (list input-file output-file stderr-file))
          (when (file-exists-p file)
            (delete-file file)))))))

(defun bm-md2org--extract-single-h1-title (text)
  "Return (TITLE . BODY) if TEXT has exactly one Markdown ATX H1.

Only headings of the form:

  # Title

are considered.  Setext headings are intentionally ignored."
  (let ((case-fold-search nil)
        matches
        (pos 0))
    (while (string-match "^#\\s-+\\(.+?\\)\\s-*#*\\s-*$" text pos)
      (push (list :beg (match-beginning 0)
                  :end (match-end 0)
                  :title (string-trim (match-string 1 text)))
            matches)
      (setq pos (match-end 0)))
    (when (= (length matches) 1)
      (let* ((match (car matches))
             (beg (plist-get match :beg))
             (end (plist-get match :end))
             (title (plist-get match :title))
             ;; Also remove one following newline if present, so we do not
             ;; leave an extra blank/partial heading behind.
             (remove-end
              (if (and (< end (length text))
                       (eq (aref text end) ?\n))
                  (1+ end)
                end))
             (body (concat (substring text 0 beg)
                           (substring text remove-end))))
        (cons title body)))))

(defun bm-md2org--promote-org-headings (org-text)
  "Promote all Org headings in ORG-TEXT by one level.

A heading like ** Foo becomes * Foo.  Top-level headings remain top-level."
  (with-temp-buffer
    (insert org-text)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*\\{2,\\}\\)\\s-+" nil t)
      (replace-match
       (substring (match-string 1) 1)
       t
       t
       nil
       1))
    (buffer-string)))

(defun bm-md2org--marker-line-p (line)
  "Return marker text if LINE is an HTML marker comment.

For example, this line:

  <!-- gh-issues:start -->

returns:

  \"gh-issues:start\""
  (when (string-match
         "\\`[[:space:]]*<!--[[:space:]]-*\\([^>]+?\\)[[:space:]]-*-->[[:space:]]*\\'"
         line)
    (string-trim (match-string 1 line))))

(defun bm-md2org--split-marker (marker)
  "Return (NAME KIND) for MARKER.

Recognised named markers are:

  NAME:start
  NAME:end

Also recognised anonymous markers are:

  start
  end

Anonymous markers return nil as NAME."
  (cond
   ((string-match "\\`\\(.+\\):\\(start\\|end\\)\\'" marker)
    (list (match-string 1 marker)
          (match-string 2 marker)))
   ((member marker '("start" "end"))
    (list nil marker))
   (t
    nil)))

(defun bm-md2org--example-block (body &optional name)
  "Return BODY wrapped as an Org example block.

If NAME is non-nil, add a preceding #+name line."
  (concat
   (when name
     (format "#+name: %s\n" name))
   "#+begin_example\n"
   (string-remove-suffix "\n" body)
   "\n#+end_example\n\n"))

(defun bm-md2org--convert-with-example-regions (markdown-text)
  "Convert MARKDOWN-TEXT to Org, preserving marked regions as example blocks.

Marked regions are delimited by lines like:

  <!-- gh-issues:start -->
  ...
  <!-- gh-issues:end -->

The body between the markers is preserved literally inside an Org example
block.  If the start and end markers use the same NAME, the block gets:

  #+name: NAME

Anonymous <!-- start --> / <!-- end --> markers are also supported, but do not
receive a #+name line."
  (let ((lines (split-string markdown-text "\n"))
        normal-lines
        example-lines
        in-example
        example-name
        output-parts)

    (cl-labels
        ((flush-normal
          ()
          (when normal-lines
            (let ((chunk (mapconcat #'identity (nreverse normal-lines) "\n")))
              (unless (string-empty-p (string-trim chunk))
                (push (bm-md2org--pandoc-string (concat chunk "\n"))
                      output-parts)))
            (setq normal-lines nil)))
         (flush-example
          ()
          (push (bm-md2org--example-block
                 (concat (mapconcat #'identity (nreverse example-lines) "\n")
                         "\n")
                 example-name)
                output-parts)
          (setq example-lines nil
                example-name nil
                in-example nil)))

      (dolist (line lines)
        (let* ((marker (bm-md2org--marker-line-p line))
               (parts (and marker (bm-md2org--split-marker marker)))
               (name (nth 0 parts))
               (kind (nth 1 parts)))
          (cond
           ;; Start of an example region.
           ((and parts (string= kind "start") (not in-example))
            (flush-normal)
            (setq in-example t
                  example-name name
                  example-lines nil))

           ;; End of a matching example region.
           ((and parts
                 (string= kind "end")
                 in-example
                 (equal name example-name))
            (flush-example))

           ;; Any other line while inside an example region is preserved.
           (in-example
            (push line example-lines))

           ;; Otherwise it is normal Markdown.
           (t
            (push line normal-lines)))))

      ;; If a start marker was never closed, do not silently drop content.
      ;; Treat the opening marker and body as normal Markdown-ish text.
      (when in-example
        (push (format "<!-- %s:start -->" example-name) normal-lines)
        (dolist (line (nreverse example-lines))
          (push line normal-lines))
        (setq in-example nil
              example-lines nil
              example-name nil))

      (flush-normal))

    (mapconcat #'identity (nreverse output-parts) "")))

(defun bm-md2org-convert-string (markdown-text &optional single-h1-to-title)
  "Convert MARKDOWN-TEXT to Org.

When SINGLE-H1-TO-TITLE is non-nil, convert a single Markdown H1 to #+TITLE
and promote remaining Org headings by one level."
  (let* ((title-body
          (and single-h1-to-title
               (bm-md2org--extract-single-h1-title markdown-text)))
         (title (car-safe title-body))
         (body (or (cdr-safe title-body) markdown-text))
         (org-body (bm-md2org--convert-with-example-regions body)))
    (when title-body
      (setq org-body (bm-md2org--promote-org-headings org-body)))
    (concat
     (when title
       (format "#+TITLE: %s\n\n" title))
     (string-trim-right org-body)
     "\n")))

(defun bm-md2org--find-example-regions (text)
  "Find marker-delimited example regions in TEXT.

Return a list of plists with keys:

  :start-beg
  :start-end
  :end-beg
  :end-end
  :name
  :body

The :name value is nil unless the markers are exactly NAME:start and NAME:end
with the same NAME."
  (let (starts regions)
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward bm-md2org-example-marker-regexp nil t)
        (let* ((marker (match-string-no-properties 1))
               (parts (bm-md2org--split-marker marker))
               (name (nth 0 parts))
               (kind (nth 1 parts))
               (line-beg (line-beginning-position))
               (line-end (min (point-max) (1+ (line-end-position)))))
          (cond
           ((string= kind "start")
            (push (list :name name
                        :start-beg line-beg
                        :start-end line-end)
                  starts))
           ((string= kind "end")
            (let* ((matching-start
                    (cl-find-if
                     (lambda (start)
                       (equal (plist-get start :name) name))
                     starts))
                   (start-name (and matching-start
                                    (plist-get matching-start :name))))
              (when matching-start
                (setq starts (delq matching-start starts))
                (push (list :start-beg (plist-get matching-start :start-beg)
                            :start-end (plist-get matching-start :start-end)
                            :end-beg line-beg
                            :end-end line-end
                            :name start-name
                            :body (buffer-substring-no-properties
                                   (plist-get matching-start :start-end)
                                   line-beg))
                      regions))))))))
    (sort regions
          (lambda (a b)
            (< (plist-get a :start-beg)
               (plist-get b :start-beg))))))


(defun bm-md2org--output-file-for-current-buffer ()
  "Return the .org output path for the current Markdown buffer."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((input-file (file-truename buffer-file-name)))
    (cond
     ((string-match-p "\\.md\\'" input-file)
      (replace-regexp-in-string "\\.md\\'" ".org" input-file))
     ((string-match-p "\\.markdown\\'" input-file)
      (replace-regexp-in-string "\\.markdown\\'" ".org" input-file))
     (t
      (user-error "Current file does not end in .md or .markdown")))))

;;;###autoload
(defun bm/md-buffer-to-org-file (&optional disable-title-conversion)
  "Convert the current Markdown buffer to a sibling Org file.

The output path is made by replacing a final .md or .markdown extension with
.org.

By default, `bm-md2org-single-h1-to-title' controls whether a single Markdown
H1 is converted to #+TITLE.

With prefix argument DISABLE-TITLE-CONVERSION, do not convert a single H1 to
#+TITLE for this invocation."
  (interactive "P")
  (when (buffer-modified-p)
    (save-buffer))
  (let* ((output-file (bm-md2org--output-file-for-current-buffer))
         (single-h1-to-title
          (and bm-md2org-single-h1-to-title
               (not disable-title-conversion)))
         (markdown-text
          (buffer-substring-no-properties (point-min) (point-max)))
         (org-text
          (bm-md2org-convert-string markdown-text single-h1-to-title)))
    (when (and (file-exists-p output-file)
               (not (yes-or-no-p (format "Overwrite %s? " output-file))))
      (user-error "Refusing to overwrite %s" output-file))
    (with-temp-file output-file
      (insert org-text))
    (message "Wrote %s" output-file)
    (find-file output-file)))

;;;###autoload
(defun bm/md-region-to-org-popup (beg end &optional disable-title-conversion)
  "Convert active Markdown region from BEG to END and show Org in a popup buffer.

With prefix argument DISABLE-TITLE-CONVERSION, do not convert a single H1 to
#+TITLE for this invocation."
  (interactive "r\nP")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((single-h1-to-title
          (and bm-md2org-single-h1-to-title
               (not disable-title-conversion)))
         (markdown-text
          (buffer-substring-no-properties beg end))
         (org-text
          (bm-md2org-convert-string markdown-text single-h1-to-title))
         (buf (get-buffer-create "*md2org output*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert org-text)
        (goto-char (point-min))
        (org-mode)
        (read-only-mode 1)))
    (display-buffer-in-side-window
     buf
     '((side . right)
       (slot . 0)
       (window-width . 0.45)))))

(provide 'bm-md2org)

;;; bm-md2org.el ends here
