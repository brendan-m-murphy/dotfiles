;;; org-gh.el --- The gh: prefix for org-mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'ol)
(eval-when-compile (require 'subr-x))

(org-link-set-parameters "gh"
                         :follow #'org-gh--follow
                         :insert-description #'org-gh--insert-description
                         :export #'org-gh--export)

;; gh:org/repo#num is much cleaner then https://github.com/org/repo/issues/num, but the
;; internet tends to provide links in the less clean form.
;;
;; This fixes up urls before `org-insert-link' is called to add a description. Since
;; "gh:" style links know how to insert their own description, this makes it possible to
;; insert a GH description by simply `yank'ing in a GH link and calling
;; `org-insert-link'.
(advice-add #'org-insert-link :before #'org-gh--fixup-http)

(defun org-gh--fixup-http (&rest _)
  "When `org-insert-link' is run, fixup http(s?) links into the gh: format.

A link gh: compatible when it is of the form

    http(s)://www.github.com/${ORG}/${REPO}/(pull|issues)/${NUMBER}

The above link can be equivalently represented as

    gh:${ORG}/${REPO}#${NUMBER}

This function performs the fixup in place."
(when (derived-mode-p 'org-mode)
  (when (org-in-regexp org-link-plain-re)
    (let ((remove (list (match-beginning 0) (match-end 0)))
          (link (org-link-unescape (match-string-no-properties 0)))
          (gh-link-regexp
           "^https?://\\(www\\.\\)?github.com/\\([^/.]+\\)/\\([^/.]+\\)/\\(pull\\|issues\\)/\\([0-9]+\\)$")
          (case-fold-search nil))
      (when (string-match gh-link-regexp link)
        (apply #'delete-region remove)
        (insert (concat "gh:"
                        (match-string 2 link) "/"
                        (match-string 3 link) "#"
                        (match-string 5 link))))))))

(defun org-gh--parse (link)
  "Parse a gh: LINK of the form gh:org/repo#issue into (org/repo . issue)."
  (let ((parts (string-split (string-remove-prefix "gh:" link) "#" t)))
    (unless (length= parts 2)
      (error "Invalid gh: link: %s" link))
    parts))

(defun org-gh--expand (link)
  "Expand LINK to the full HTTPS link."
  (apply #'format "https://www.github.com/%s/issues/%s" (org-gh--parse link)))

(defun org-gh--follow (link _)
  "Open a `gh' based LINK of the format gh:org/repo#number."
  (org-link-open-from-string (org-gh--expand link)))

(defun org-gh--export (link desc backend info)
  "Export an gh: type (LINK DESC) by invoking the BACKEND's link transcoder.

INFO is passed to the transcoder."
  ;; Both `ox' and `org-element-ast' must have been invoked for the exporter to
  ;; run.
  (require 'ox)
  (require 'org-element-ast)
  (if-let ((transcoder (alist-get 'link (org-export-backend-transcoders
                                         (org-export-get-backend backend)))))
      (let ((link (org-element-create
                   'link (list :type "https"
	                       :path (org-gh--expand link)))))
        (funcall transcoder link desc info))
    (concat "gh:" link)))

(defun org-gh--insert-description (link description)
  "Find the name of a GH issue for display purposes.
LINK is a gh link of the form org/repo#number.
DESCRIPTION is the existing description."
  (or description
      (when-let ((title (apply #'org-gh--get-issue-title (org-gh--parse link))))
       (format "%s: %s" (string-remove-prefix "gh:" link) title))))

(defun org-gh--get-issue-title (repo issue)
  "Get the title associated with the (REPO . ISSUE) pair."
  (when-let ((gh (executable-find "gh")))
    (with-temp-buffer
      (unless (equal 0 (call-process
                        gh nil t nil
                        "issue" "view" issue
                        "--repo" repo
                        "--json" "title"))
        (user-error "Failed to get title from GH: %s"
                    (progn (goto-char (point-min))
                           (buffer-string))))
      (goto-char (point-min))
      (alist-get 'title (json-parse-buffer :object-type 'alist)))))

(provide 'org-gh)

;;; org-gh.el ends here
