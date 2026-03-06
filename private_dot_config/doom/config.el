;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Brendan Murphy"
      user-mail-address "hello@brendanmurphy.org")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; spacer window for wide monitor
(defun my/create-left-side-window ()
  "Create a persistent 1/9 width window on the left."
  (interactive)
  (let ((window (display-buffer-in-side-window
                 (get-buffer-create "*left-side*")
                 '((side . left)
                   (slot . 0)
                   (window-width . 0.11)))))
    (set-window-dedicated-p window t)))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (list (concat org-directory "todo.org")
                             (concat org-directory "work/todo.org")
                             (concat org-directory "meeting_notes.org")
                             (concat org-directory "important_dates.org")
                             (concat org-directory "renovation.org")
                             (concat org-directory "home.org")
                             (concat org-directory "work/")
                             (concat org-directory "projects/")
                             ))

;; remove archive
(setq org-agenda-files (remove (concat org-directory "archive/") org-agenda-files))

(map! :after org
      "C-c C-w" #'org-refile)

(setq org-refile-targets
      `((nil . (:maxlevel . 6))
        (org-agenda-files . (:maxlevel . 3))
        (,(directory-files (concat org-directory "work/") 'full (rx ".org" eos)) . (:maxlevel . 3))
        ))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJ(p)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)" )
        (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))

;; previous value:
;; ((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
;;  (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
;;  (sequence "|" "OKAY(o)" "YES(y)" "NO(n)"))


;; org github links
(use-package! org-gh
  :after org)


;; BASIC CONFIG
(setq doom-font-increment 1)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; mac OSX use right alt as meta
(setq mac-right-option-modifier 'meta)


;; exec-path-from-shell
;; https://github.com/purcell/exec-path-from-shell
;; TODO: this isn't working??
(use-package! exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; TRAMP
;; (setq tramp-default-method "ssh")
;; (setq tramp-default-user "bm13805")
;; (setq tramp-default-host "bp1")
(defun bp (x)
  "Open dired buffer for directory. "
  (interactive "sBlue Pebble directory: ")
  (let ((path
         (cond ((string= x "home") "/user/home/bm13805/")
               ((string= x "work") "/user/work/bm13805/")
               ((string= x "acrg") "/group/chem/acrg/")
               )
         ))
    (find-file (format "/sshx:bm13805@bp1:%s" path))
    (rename-buffer (format "Blue Pebble %s" x))
    )
  )

(defun disable-company-on-remote-buffers ()
  "Disable company in current buffer, if it is remote."
  (when (and (fboundp 'company-mode)
             (file-remote-p default-directory))
    (company-mode -1)))

;; disable company on remote shells
(add-hook! shell-mode 'disable-company-on-remote-buffers)

(after! tramp
  (setq projectile--mode-line "Projectile")
  (setq tramp-debug-buffer t) ;; not sure this works?
  (setq tramp-verbose 1) ;; increase for more info
  (dolist (path '("/sw/tools/git-2.35.1/bin" "/system/slurm/23.02.4/bin" "/user/home/bm13805/bin"))
    (add-to-list 'tramp-remote-path path))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:bm13805@bp1:")
                     "remote-shell" "/bin/sh"))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/sshx:bm13805@bp1:")
                     "remote-shell-login" "-l"))
                                        ;(customize-set-variable 'tramp-encoding-shell "/bin/sh") ;; /bin/sh is default..
  )


;; PYTHON
(use-package! pyvenv
  :config
                                        ;(pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat (replace-regexp-in-string ".*:" "" pyvenv-virtual-env) "bin/python")))
              (lambda ()
                (let ((ipython-path (concat (replace-regexp-in-string ".*:" "" pyvenv-virtual-env) "bin/ipython")))
                  (setq +python-ipython-command (list ipython-path "-i" "--simple-prompt" "--no-color-info"))))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python"))
              (lambda ()
                (setq +python-ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info"))))))

(use-package! python-black
  :demand t
  :after python
  :config
  ;; (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  (setq python-black-extra-args '("-l 110"))
  ;; Feel free to throw your own personal keybindings here
  ;;(map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  ;;(map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  ;;(map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)
  )


;;; micromamba
(use-package! micromamba
  :after python)

;;; ropemacs
;;(setq ropemacs-enable-shortcuts nil)
;;(setq repemacs-local-prefix "C-c C-a")
;;(use-package! pymacs
;;  :load-path "lisp/pymacs.el")
;;(defun load-ropemacs ()
;;  "Load pymacs and ropemacs"
;;  (interactive)
;;  (require 'pymacs)
;;  (pymacs-load "ropemacs" "rope-")
;;  ;; Automatically save project python buffers before refactorings
;;  (setq ropemacs-confirm-saving 'nil)
;;)

;; JUPYTER
(use-package! zmq)

(map! :after org
      "C-c i j" #'jupyter-org-insert-src-block)

(defun display-ansi-colors ()
  (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'org-babel-after-execute-hook #'display-ansi-colors)

(with-eval-after-load 'ob-jupyter
  (org-babel-jupyter-aliases-from-kernelspecs))

;;; fix for warning after M-x, then typing "org" from a org-mode buffer
(defun my-jupyter-org--define-key-filter (orig &rest args)
  (if (derived-mode-p 'org-mode)
      (apply orig args)
    ;; If we're not in org-mode (e.g. *temp* during doc/annotation),
    ;; just return nil or the unfiltered bindings, depending on what the
    ;; filter is expected to do.
    nil))

(after! jupyter
  (advice-add 'jupyter-org--define-key-filter :around #'my-jupyter-org--define-key-filter))

;; MAGIT

;; SMERGE
(defun smerge-repeatedly ()
  "Perform smerge actions again and again"
  (interactive)
  (smerge-mode 1)
  (smerge-transient))

(after! transient
  (transient-define-prefix smerge-transient ()
    [["Move"
      ("n" "next" (lambda () (interactive) (ignore-errors (smerge-next)) (smerge-repeatedly)))
      ("p" "previous" (lambda () (interactive) (ignore-errors (smerge-prev)) (smerge-repeatedly)))]
     ["Keep"
      ("b" "base" (lambda () (interactive) (ignore-errors (smerge-keep-base)) (smerge-repeatedly)))
      ("u" "upper" (lambda () (interactive) (ignore-errors (smerge-keep-upper)) (smerge-repeatedly)))
      ("l" "lower" (lambda () (interactive) (ignore-errors (smerge-keep-lower)) (smerge-repeatedly)))
      ("a" "all" (lambda () (interactive) (ignore-errors (smerge-keep-all)) (smerge-repeatedly)))
      ("RET" "current" (lambda () (interactive) (ignore-errors (smerge-keep-current)) (smerge-repeatedly)))]
     ["Diff"
      ("<" "upper/base" (lambda () (interactive) (ignore-errors (smerge-diff-base-upper)) (smerge-repeatedly)))
      ("=" "upper/lower" (lambda () (interactive) (ignore-errors (smerge-diff-upper-lower)) (smerge-repeatedly)))
      (">" "base/lower" (lambda () (interactive) (ignore-errors (smerge-diff-base-lower)) (smerge-repeatedly)))
      ("R" "refine" (lambda () (interactive) (ignore-errors (smerge-refine)) (smerge-repeatedly)))
      ("E" "ediff" (lambda () (interactive) (ignore-errors (smerge-ediff)) (smerge-repeatedly)))]
     ["Other"
      ("c" "combine" (lambda () (interactive) (ignore-errors (smerge-combine-with-next)) (smerge-repeatedly)))
      ("r" "resolve" (lambda () (interactive) (ignore-errors (smerge-resolve)) (smerge-repeatedly)))
      ("k" "kill current" (lambda () (interactive) (ignore-errors (smerge-kill-current)) (smerge-repeatedly)))
      ("q" "quit" (lambda () (interactive) (smerge-auto-leave)))]]))


(add-hook! magit-diff-visit-file-hook (smerge-repeatedly))


;; C/C++
(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=3"
          ;;"--include-directory=/Users/bm13805/.pyenv/versions/3.10.14/include/python3.10/"
          ;;"--enable-config"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (setq lsp-clients-clangd-library-directories
        '("/usr"
          "/Users/bm13805/.pyenv/versions/3.10.14/include/python3.10/"
          "/Users/bm13805/Documents/pytensor_fork/.venv/include"))
  (set-lsp-priority! 'clangd 2))


;; org mode
;; (after! org
;;   ;; org-reveal
;;   (load-library "ox-reveal")
;;   (setq org-reveal-root "file:///Users/bm13805/bin/reveal.js")
;;   )


;; fix for macOS "file limit" error
(defun file-notify-rm-all-watches ()
  "Remove all existing file notification watches from Emacs."
  (interactive)
  (maphash
   (lambda (key _value)
     (file-notify-rm-watch key))
   file-notify-descriptors))

;; lsp issues
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.site-packages\\'")
  )

;; mermaid
(use-package! mermaid-mode)
(setq ob-mermaid-cli-path "/opt/homebrew/bin/mmdc")


;; magit forge
(setq auth-sources '("~/.authinfo.gpg"))


;; orgit/orgit-forge
(use-package! orgit)
(use-package! orgit-forge)


;; google docstrings for python
(use-package! googledocstrings
  :config
  (setq googledocstrings-insert-examples-block nil)
  (setq googledocstrings-insert-parameter-types nil)
  )


;; code cell minor mode for jupytext (or ipynb)
(use-package! code-cells
  :after python)


;; gptel
(defvar my/openai-api-key--cache nil)

(defun my/openai-api-key ()
  "Fetch OpenAI API key from macOS Keychain (cached)."
  (or my/openai-api-key--cache
      (setq my/openai-api-key--cache
            (string-trim
             (shell-command-to-string
              (format "security find-generic-password -a %s -s openai-api-key -w"
                      (shell-quote-argument user-login-name)))))))

;; custom OpenAI backend object with more recent model list (as of 26 Jan 2026)
(after! gptel
  ;; Define an explicit OpenAI API backend whose model allowlist includes gpt-5.2.
  ;; This avoids mutating internal backend structs (which may not have setf accessors).
  (setq gptel-backend
        (gptel-make-openai "OpenAI API"
          :key #'my/openai-api-key
          :models '(gpt-5.2
                    gpt-5.1 gpt-5 gpt-5-mini gpt-5-nano
                    gpt-4.1 gpt-4.1-mini gpt-4.1-nano
                    gpt-4o gpt-4o-mini
                    o1 o1-mini o3 o3-mini o4-mini
                    gpt-4-turbo gpt-4 gpt-3.5-turbo)))

  (setq gptel-model 'gpt-5.2))

(after! gptel
  ;; (setq! gptel-api-key #'my/openai-api-key
  ;;        gptel-model 'gpt-5.2)

  (setq!       gptel-default-mode 'org-mode)

  ;; org-mode prompt/response prefixes
  ;; Ensure these are alists (and avoid mutating a shared constant)
  (setq gptel-prompt-prefix-alist
        (if (listp gptel-prompt-prefix-alist)
            (copy-alist gptel-prompt-prefix-alist)
          nil)
        gptel-response-prefix-alist
        (if (listp gptel-response-prefix-alist)
            (copy-alist gptel-response-prefix-alist)
          nil))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n"
        (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")

  ;; Enable gptel's optional convenience integrations, including MCP helpers.
  (require 'gptel-integrations)

  ;; Ensure MCP client library is available (installed via packages.el).
  (require 'mcp nil 'noerror)

  ;; Optional: handy bindings (pick your own keys)
  ;; (map! :leader
  ;;       (:prefix ("a i" . "AI")
  ;;        :desc "gptel tools menu"      "t" #'gptel-tools
  ;;        :desc "MCP connect"          "c" #'gptel-mcp-connect
  ;;        :desc "MCP disconnect"       "d" #'gptel-mcp-disconnect))
  )

;; gptel directives
(after! gptel
  ;; Keep 'default' directive intact; add a new one for your policy.
  (add-to-list 'gptel-directives
               '(retrieval-policy .
                 "Retrieval policy (strict):
- Do NOT read entire files by default.
- First use: glob → grep (-n) to identify the minimum relevant locations.
- Only extract small snippets around matches (±30 lines). Prefer head/tail.
- Prefer summaries/indices over raw logs/CSVs; never ingest large tables wholesale.
- If you must read a file, justify why and keep it to the smallest necessary region.
- Always cite evidence as file:line-range.")))

;; GPTEL TOOLS
(after! gptel
  (use-package! gptel-agent
    :config
    ;; Register gptel-agent’s tools + presets (web, local search/files, bash, etc.)
    (gptel-agent-update)))


(after! gptel
  (use-package! llm-tool-collection
    :defer t
    :config
    ;; Register just the filesystem + search categories for gptel:
    ;; This is targeted and keeps the tool surface small.
    (mapcar (apply-partially #'apply #'gptel-make-tool)
            (llm-tool-collection-get-category "filesystem"))
    (mapcar (apply-partially #'apply #'gptel-make-tool)
            (llm-tool-collection-get-category "search"))))

;;; Local tools
(add-to-list 'load-path (expand-file-name "lisp" doom-user-dir))
(require 'my-gptel-tools)
(require 'my-gptel-org-workflow)
(after! gptel
  (my/gptel-register-tools)

  ;; Make local filesystem + buffer tools the default active toolset.
  ;; These names must match the :name fields in my-gptel-tools.el.
  (setq gptel-tools
        (mapcar #'gptel-get-tool
                '(
                  ;; Filesystem tools
                  "list_files"
                  "rg"
                  "read_range"
                  "head"
                  "tail"

                  ;; Buffer tools
                  "list_relevant_buffers"
                  "read_buffer_range"
                  "search_buffer"
                  )))

  (my/gptel-org-workflow-enable)

  ;; Optional: use hierarchical Org context
  (setq gptel-use-context 'org)

  (map! :leader
        :desc "New GPT conversation entry"
        "a n" #'my/gptel-new-entry)
  (map! :leader
        :desc "New GPT topic"
        "a t" #'my/gptel-new-topic)
  (map! :leader
        :desc "gptel add reference (this file)" "a r" #'my/gptel-add-reference
        :desc "gptel add reference (open buffers)" "a R" #'my/gptel-add-reference-from-open-buffers
        :desc "gptel insert reference block" "a b" #'my/gptel-insert-reference-block)
  (map! :leader
        :desc "Mark buffer relevant" "a m" #'my/gptel-mark-buffer-relevant
        :desc "Unmark buffer relevant" "a u" #'my/gptel-unmark-buffer-relevant
        :desc "Show relevant buffers" "a M" #'my/gptel-show-relevant-buffers
        :desc "Clear relevant buffers" "a C" #'my/gptel-clear-relevant-buffers
        :desc "Mark project buffers relevant" "a p" #'my/gptel-mark-project-buffers-relevant))

(use-package! macher
  :after gptel
  :custom
  (macher-action-buffer-ui 'org)
  :config
  (macher-install)
  (macher-enable))


;; ;;; gptel tools (local dev)
;; (let ((gptel-tools-dir (expand-file-name "~/Documents/gptel-tools")))
;;   (when (file-directory-p gptel-tools-dir)
;;     (add-to-list 'load-path gptel-tools-dir)))

;; ;; Do not hard-fail startup/doom doctor if local dev code is broken or API changed.
;; (with-eval-after-load 'gptel
;;   (condition-case err
;;       (progn
;;         (require 'gptel-tools nil 'noerror)
;;         ;; Call setup only if it exists (your code may be mid-refactor).
;;         (when (fboundp 'gptel-tools-setup)
;;           (gptel-tools-setup)))
;;     (error
;;      (message "[gptel-tools] disabled due to error: %S" err))))



;;; ---------------------------------------------------------------------------
;;; Magit worktree helpers + Codex integration
;;;
;;; Assumptions
;;; - main repo lives at:   ~/Documents/<repo>
;;; - worktrees live at:    ~/Documents/<repo>-wt/<branch>
;;; - Codex macOS app is installed and callable via: open -a Codex
;;;
;;; Commands provided:
;;;
;;; C-c g t   create a new task worktree (branch + magit)
;;; C-c g c   create worktree and open it in Codex
;;; C-c g o   open current directory in Codex
;;;
;;; ---------------------------------------------------------------------------

;; (require 'magit)

;; (defun my/repo-default-branch ()
;;   "Return the repository default branch (origin/HEAD), fallback to 'main'."
;;   (or
;;    (magit-git-string "symbolic-ref" "--short" "refs/remotes/origin/HEAD")
;;    "main"))

;; (defun my/open-in-codex (&optional dir)
;;   "Open DIR (or current directory) in the Codex macOS app."
;;   (interactive)
;;   (let ((path (or dir default-directory)))
;;     (start-process "open-codex" nil "open" "-a" "Codex" path)
;;     (message "Opening Codex in %s" path)))

;; (defun my/magit-new-task-worktree (name &optional open-codex)
;;   "Create worktree NAME under ../<repo>-wt/, create branch, and open Magit.

;; If OPEN-CODEX is non-nil, also open the directory in the Codex app."
;;   (interactive
;;    (list
;;     (read-string "Worktree / branch name: ")
;;     current-prefix-arg))

;;   (let* ((repo (magit-toplevel))
;;          (repo-name (file-name-nondirectory (directory-file-name repo)))
;;          (parent (file-name-directory (directory-file-name repo)))
;;          (wt-root (expand-file-name (format "%s-wt" repo-name) parent))
;;          (wt-dir (expand-file-name name wt-root))
;;          (default-branch (my/repo-default-branch))
;;          (default-directory repo))

;;     (make-directory wt-root t)

;;     (message "Creating worktree %s from %s..." name default-branch)

;;     (magit-call-git
;;      "worktree" "add"
;;      "-b" name
;;      wt-dir
;;      default-branch)

;;     ;; open Magit in the new worktree
;;     (magit-status wt-dir)

;;     ;; optionally open Codex
;;     (when open-codex
;;       (my/open-in-codex wt-dir))))

;; (defun my/magit-new-worktree-and-codex (name)
;;   "Create worktree NAME and open it in Codex."
;;   (interactive "sWorktree / branch name: ")
;;   (my/magit-new-task-worktree name t))

;; ;; Keybindings
;; (global-set-key (kbd "C-c g t") #'my/magit-new-task-worktree)
;; (global-set-key (kbd "C-c g c") #'my/magit-new-worktree-and-codex)
;; (global-set-key (kbd "C-c g o") #'my/open-in-codex)

;;; ---------------------------------------------------------------------------
;;; Magit worktree + uv helpers + Codex integration
;;; ---------------------------------------------------------------------------

(after! magit

  (defun my/repo-default-branch ()
    "Return repository default branch (origin HEAD) or fallback to 'main'."
    (or
     (let ((ref (magit-git-string "symbolic-ref" "--short" "refs/remotes/origin/HEAD")))
       (when ref (string-remove-prefix "origin/" ref)))
     "main"))

  (defun my/open-in-codex (&optional dir)
    "Open DIR (or current directory) in the Codex macOS app."
    (interactive)
    (let ((path (or dir default-directory)))
      (start-process "open-codex" nil "open" "-a" "Codex" path)
      (message "Opening Codex in %s" path)))

  (defun my/uv-sync-here (&optional dir)
    "Run `uv sync` in DIR (or current directory)."
    (interactive)
    (let ((path (or dir default-directory)))
      (start-process
       "uv-sync"
       "*uv-sync*"
       "bash" "-lc"
       (format "cd %s && uv sync" (shell-quote-argument path)))
      (message "Running uv sync in %s" path)))

  (defun my/magit-new-task-worktree (name &optional open-codex)
    "Create worktree NAME under ../<repo>-wt/, branch from default branch."
    (interactive
     (list
      (read-string "Worktree / branch name: ")
      current-prefix-arg))

    (let* ((repo (magit-toplevel))
           (repo-name (file-name-nondirectory (directory-file-name repo)))
           (parent (file-name-directory (directory-file-name repo)))
           (wt-root (expand-file-name (format "%s-wt" repo-name) parent))
           (wt-dir (expand-file-name name wt-root))
           (default-branch (my/repo-default-branch))
           (default-directory repo))

      (make-directory wt-root t)

      (message "Creating worktree %s from %s..." name default-branch)

      (magit-call-git
       "worktree" "add"
       "-b" name
       wt-dir
       default-branch)

      ;; open Magit for new worktree
      (magit-status wt-dir)

      ;; optionally open Codex
      (when open-codex
        (my/open-in-codex wt-dir))))

  (defun my/magit-new-worktree-and-codex (name)
    "Create worktree NAME and open it in Codex."
    (interactive "sWorktree / branch name: ")
    (my/magit-new-task-worktree name t))

  (defun my/magit-worktree-merged-p (branch)
    "Return t if origin/BRANCH does not exist (i.e., branch deleted upstream)."
    (not (magit-git-success
          "ls-remote" "--exit-code" "--heads" "origin" branch)))

  (defun my/magit-cleanup-worktree ()
    "Remove the current worktree and delete its local branch if the remote branch is gone."
    (interactive)
    (let* ((branch (magit-get-current-branch))
           (dir default-directory)
           (repo (magit-toplevel))
           (default-directory repo))

      (unless branch
        (user-error "Not on a branch"))

      ;; refresh remote state
      (magit-call-git "fetch" "origin")

      (if (my/magit-worktree-merged-p branch)
          (when (yes-or-no-p
                 (format "Remote branch '%s' deleted. Remove worktree and local branch? " branch))

            ;; remove worktree
            (magit-call-git "worktree" "remove" "--force" dir)

            ;; delete local branch
            (magit-call-git "branch" "-D" branch)

            ;; clean stale metadata
            (magit-call-git "worktree" "prune")

            (message "Cleaned up worktree and branch %s" branch))

        (message
         "Remote branch origin/%s still exists — PR may not be merged yet." branch)))))

;; Keybindings

(map! :leader
      (:prefix ("g" . "git")
       :desc "Create worktree" "t" #'my/magit-new-task-worktree
       :desc "Create worktree + Codex" "c" #'my/magit-new-worktree-and-codex
       :desc "Run uv sync" "v" #'my/uv-sync-here
       :desc "Open in Codex" "o" #'my/open-in-codex
       :desc "Cleanup merged worktree" "x" #'my/magit-cleanup-worktree))
