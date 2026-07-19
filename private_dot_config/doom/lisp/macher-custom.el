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
    (mapcar (lambda (root)
              (file-name-as-directory (expand-file-name root)))
            (delq nil roots))))

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
        (let ((dir (expand-file-name root)))
          (when (file-directory-p dir)
            (file-name-as-directory dir)))))))

(defun my/macher-root-from-allowed-roots ()
  "Return deepest matching allowed root for current buffer, or nil."
  (let* ((dir (file-name-as-directory (expand-file-name default-directory)))
         (matches (cl-remove-if-not
                   (lambda (root)
                     (string-prefix-p root dir))
                   (my/macher--allowed-roots))))
    (car (sort matches (lambda (a b) (> (length a) (length b)))))))

(defun my/macher-resolve-workspace-root ()
  "Resolve macher workspace root from configured heuristics."
  (or (my/macher-root-from-org)
      (my/macher-root-from-allowed-roots)))

(defun my/gptel-set-macher-workspace ()
  "Set `macher--workspace` for current buffer when a root is available."
  (when-let ((root (my/macher-resolve-workspace-root)))
    (setq-local macher--workspace
                (cons 'project (file-name-as-directory root)))))

(defun my/gptel-send-with-macher-workspace (orig-fn &rest args)
  "Ensure macher workspace is set before running ORIG-FN with ARGS."
  (my/gptel-set-macher-workspace)
  (apply orig-fn args))

(defun my/org-set-macher-root (dir)
  "Set MACHER_ROOT property for current Org entry to DIR."
  (interactive "DRepository/worktree: ")
  (org-set-property
   "MACHER_ROOT"
   (file-name-as-directory (expand-file-name dir))))

(defun my/org-set-macher-root-from-project ()
  "Set MACHER_ROOT from current project root, or prompt for directory."
  (interactive)
  (let* ((proj (and (fboundp 'project-current)
                    (project-current nil)))
         (dir (if (and proj (fboundp 'project-root))
                  (project-root proj)
                (read-directory-name "Repository/worktree: "))))
    (my/org-set-macher-root dir)))

(with-eval-after-load 'gptel
  (advice-add 'gptel-send :around #'my/gptel-send-with-macher-workspace))

(provide 'macher-custom)
;;; macher-custom.el ends here
