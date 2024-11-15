;;; init --- my .emacs.d/init.el
;;; Commentary:

;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; (package-initialize)

;; TODO:
;; - Consolidate defuns in their own file maybe?

;; Load up customizations from their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable package.el stuff per https://github.com/radian-software/straight.el#getting-started
(setq package-enable-at-startup nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; set usernames for source forges
(setq straight-host-usernames
        '((github . "geoffp")
          (gitlab . "geoffp")
          (codeberg . "geoffp")
          (bitbucket . "geoffp")))

;; SSL
;; (add-to-list 'gnutls-trustfiles "/etc/ssl/certs/tgt-ca-bundle.crt")

;; Default working directory
(let ((default-dir "~/src/uxe/"))
  (when (file-directory-p default-dir)
    (setq default-directory default-dir)
    (setq command-line-default-directory default-dir)))

;; Message about native compilation
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
  (message "Native complation is *not* available"))

;; Message about native JSON
(if (functionp 'json-serialize)
  (message "Native JSON is available")
  (message "Native JSON is *not* available"))


;;
;; PACKAGE CONFIGS
;;

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :bind (("C-M-g" . 'magit-status)))

(use-package iedit)

(use-package wgrep)

(use-package editorconfig)

;; make eshell colorful
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))


;; VERTICO STUFF

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :bind (("M-o" . 'next-multiframe-window))
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))



;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))


;; Icons plz
(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :after (all-the-icons))

(use-package all-the-icons-completion
  :if (display-graphic-p)
  :after (all-the-icons marginalia)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))

(use-package markdown-mode)

(use-package string-inflection
  :bind (("C-<tab>" . 'string-inflection-java-style-cycle)))

;; TIDE: TypeScript IDE mode
;; (use-package typescript-mode)
(use-package company)

(use-package web-mode
  :mode (("\\.svg\\'" . web-mode)
         ("\\.webc\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)))

;; Pug templates!
(use-package pug-mode
  :mode "\\.pug\\'")

;; Configure stuff for eglot, js, ts, etc.
(require 'eglot)

;; Set eglot's Python language server
(setq eglot-python-server "pyright")  ;; or "pylsp" if you prefer that
;; Use eglot with flymake for on-the-fly syntax checking
(add-hook 'python-mode-hook
          (lambda ()
            (setq-local flymake-no-changes-timeout nil)
            (setq-local flymake-start-on-save-buffer nil)))

(add-to-list 'eglot-server-programs '(toml-ts-mode "taplo" "lsp" "stdio"))
(add-to-list 'eglot-server-programs '(((web-mode :language-id "javascript"))
                                      "typescript-language-server" "--stdio"))
(add-to-list 'eglot-server-programs '(((css-mode :language-id "css"))
                                      "vscode-css-language-server" "--stdio"))
;; (add-to-list 'eglot-server-programs '(((css-mode :language-id "python"))
;;   "pyright-langserver" "--stdio"))

(add-to-list 'eglot-server-programs '(((python-ts-mode :language-id "python"))
  "npx" "--yes" "--package=pyright" "--" "pyright-langserver" "--stdio"))


;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (css-mode . lsp)
;;          (css-ts-mode . lsp)
;;          (js-mode . lsp)
;;          (js-ts-mode . lsp)
;;          ;; if you want which-key integration
;;          ;; (lsp-mode . lsp-enable-which-key-integration)
;;          )
;;   :commands lsp)

;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))

(add-hook 'js-mode-hook
	        (lambda()
		        (unbind-key "M-." js-mode-map)))
(add-hook 'js-ts-mode-hook
	        (lambda()
		        (unbind-key "M-." js-ts-mode-map)))

(use-package prettier
  :hook ((typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          web-mode
          rjsx-mode
          js2-mode
          javascript-mode) . prettier-mode))

(use-package add-node-modules-path
  :config
  (eval-after-load 'markdown-mode
    '(add-hook 'markdown-mode-hook 'add-node-modules-path))
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook 'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook 'add-node-modules-path)))

;; (use-package flycheck
;;   ;; :hook (json-mode emacs-lisp-mode markdown-mode css-mode)
;;   :config (global-flycheck-mode))

(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el"))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  :load-path ("path-to-git-checkout-of-combobulate"))

;; Are these still needed, or does treesit-auto take care of them?
;; (add-to-list 'auto-mode-alist '("/Dockerfile" . dockerfile-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(use-package css-in-js-mode
  :straight '(css-in-js-mode :type git :host github
              :repo "orzechowskid/tree-sitter-css-in-js"
              :fork t))

;; (use-package visual-regexp)

;;
;; company mode, for auto-completion wherever possible.
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-RET") 'company-complete)

;; file types to open in web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
;; and in js-ts-mode

;; load up rainbow-mode in various modes in which we may find it useful
(use-package rainbow-mode
  :hook (javascript-mode typescript-mode css-mode vue-mode web-mode))

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; also disable interlock symlinks
(setq create-lockfiles nil)

;; yasnippet setup
;; Bind yas-expand to shift-tab
(use-package yasnippet
  :config
  (global-set-key (kbd "<S-tab>") 'yas-expand)
  (yas-global-mode 1))

;; load machine-local init files
(let ((f "~/.emacs.d/init.local.el"))
  (if (file-readable-p f)
      (load-file f)))

;; pretty printing commands
(defun reformat-xml ()
  "Pretty-print an XML document using xmllint."
  (interactive)
  (call-process-region (point-min) (point-max) "xmllint" t t t "--format" "-")
  (goto-char (point-min)))

;; Dired
;; Add icons to dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; Python IDE
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'elpy-mode-hook
            (lambda () (add-hook 'before-save-hook
                                 'elpy-format-code nil t)))
  )
(use-package swift-mode)

(use-package noccur)

(use-package glsl-mode)

(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "M-o") nil)))

;; This makes GhostText (Firefox) work -- for editing text in browser from Emacs
(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("shadertoy\\.com" . glsl-mode)))
  :init
  (atomic-chrome-start-server))

(use-package jsonian
  :after flycheck
  :mode ("\\.json\\'" . jsonian-mode)
  :config (jsonian-enable-flycheck))

;; 
;; Languagetool! Spell checking and such.
;; 
;; To start the server on MacOS:
;; 
;;   brew services start languagetool
;; 
;; See also https://dev.languagetool.org/http-server
(use-package languagetool)

;; Presentation mode, naturally.
(use-package presentation)

;; Edit-indirect mode: edit regions in separate buffers
(use-package edit-indirect)

;; Edit-indirect mode: edit regions in separate buffers
(use-package polymode
  :config
  (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'lsp-on-change)
  (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'lsp-before-change))

;; Major mode for editing .nix files
(use-package nix-mode)


;; AI integration
(use-package gptel
  :config
  (setq-default gptel-backend (gptel-make-openai                    ;Not a typo, same API as OpenAI
                                  "lmstudio"                           ;Any name
                                :stream t                            ;Stream responses
                                :protocol "http"
                                :host "localhost:5555"               ;Llama.cpp server location, typically localhost:8080 for Llamafile
                                :key nil                             ;No key needed
                                :models '("test"))                   ;Any names, doesn't matter for Llama
                gptel-model "test")
  )

(use-package vterm)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
                                        
;; Enable menu bar on Mac OS. Why not, after all?
(if (eq 'system-type :darwin)
    (setq menu-bar-mode t)
  (setq menu-bar-mode nil))

;; unbind M-o from HTML mode
(defun html-mode-setup ()
  "Customize keybindings for HTML mode."
  (defvar html-mode-map)
  (unbind-key "M-o" html-mode-map))
(add-hook 'html-mode-hook 'html-mode-setup)

;; alias the font window since I can't ever remember the command
(defalias 'font-window 'menu-set-font)

;; Bring in my own packages
(require 'slugify "~/.emacs.d/geoff/slugify.el")
(require 'misc "~/.emacs.d/geoff/misc.el")

(provide 'init)
;;; init.el ends here
