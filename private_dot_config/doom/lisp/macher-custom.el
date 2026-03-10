;;; lisp/macher-custom.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'org)
(require 'subr-x)

(defvar my/gptel-allowed-roots nil
  "Optional explicit allowed roots for macher workspace resolution.")

(defun my/macher--allowed-roots ()
  "Return normalized allowed roots for workspace fallback."
  (let ((roots (or my/gptel-allowed-roots
                   (when (fboundp 'my/gptel--allowed-roots)
                     (my/gptel--allowed-roots)))))
    (mapcar #'expand-file-name (delq nil roots))))

(defun my/macher-root-from-org ()
  "Return workspace root from Org MACHER_ROOT property, or nil."
  (when (derived-mode-p 'org-mode)
    (let ((root (or (org-entry-get nil "MACHER_ROOT" 'inherit)
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward
                             "^#\\+PROPERTY:[ \t]+MACHER_ROOT[ \t]+\\(.+\\)$"
                             nil t)
                        (string-trim (match-string 1)))))))
      (when (and root (not (string-empty-p root)))
        (expand-file-name root)))))

(defun my/macher-root-from-buffer-files ()
  "Infer a repo root from path-like strings in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((path-re "\\(?:~\\|/\\|[[:alnum:]_.-]+/\\)[[:alnum:]_./~-]+")
          (allowed-roots (my/macher--allowed-roots))
          repo-root)
      (while (and (not repo-root) (re-search-forward path-re nil t))
        (let* ((raw (match-string-no-properties 0))
               (candidates
                (append
                 (list (expand-file-name raw default-directory))
                 (unless (file-name-absolute-p raw)
                   (mapcar (lambda (root) (expand-file-name raw root))
                           allowed-roots)))))
          (setq repo-root
                (cl-loop for candidate in candidates
                         for root = (locate-dominating-file candidate ".git")
                         when root
                         return (expand-file-name root)))))
      repo-root)))

(defun my/macher-root-from-allowed-roots ()
  "Return deepest matching allowed root for current buffer, or nil."
  (let* ((dir (expand-file-name default-directory))
         (matches (cl-remove-if-not
                   (lambda (root)
                     (string-prefix-p (file-name-as-directory root) dir))
                   (my/macher--allowed-roots))))
    (car (sort matches (lambda (a b) (> (length a) (length b)))))))

(defun my/macher-resolve-workspace-root ()
  "Resolve macher workspace root from configured heuristics."
  (or (my/macher-root-from-org)
      (my/macher-root-from-buffer-files)
      (my/macher-root-from-allowed-roots)))

(defun my/gptel-set-macher-workspace ()
  "Set `macher--workspace` for current buffer when a root is available."
  (when-let ((root (my/macher-resolve-workspace-root)))
    (setq-local macher--workspace (cons 'project root))))

(defun my/gptel-send-with-macher-workspace (orig-fn &rest args)
  "Ensure macher workspace is set before running ORIG-FN with ARGS."
  (my/gptel-set-macher-workspace)
  (apply orig-fn args))

(advice-add 'gptel-send :around #'my/gptel-send-with-macher-workspace)

(provide 'macher-custom)
;;; macher-custom.el ends here
