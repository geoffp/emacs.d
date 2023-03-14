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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; add melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; SSL
;; (add-to-list 'gnutls-trustfiles "/etc/ssl/certs/tgt-ca-bundle.crt")

;; Default working directory
(let ((default-dir "~/src/"))
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
;; use-package config
;;

;; always install packages managed by use-package if they're missing
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;; (setq use-package-hook-name-suffix nil)



;; keep packages up to date
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; start requiring packages we need

;; (use-package dracula-theme
;;   :config
;;   (load-theme 'dracula t))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :bind (("C-M-g" . 'magit-status)))

(use-package ace-window
  :bind (("M-o" . 'ace-window)))

(use-package iedit)

(use-package counsel)

(use-package counsel-projectile
  :after (counsel))

(use-package ivy
  :after (counsel)
  :bind (
          ;; Ivy-based interface to standard commands
          ("C-s" . 'swiper-isearch)
          ("M-x" . 'counsel-M-x)
          ("C-x C-f" . 'counsel-find-file)
          ("M-y" . 'counsel-yank-pop)
          ("<f1> f" . 'counsel-describe-function)
          ("<f1> v" . 'counsel-describe-variable)
          ("<f1> l" . 'counsel-find-library)
          ("<f2> i" . 'counsel-info-lookup-symbol)
          ("<f2> u" . 'counsel-unicode-char)
          ("<f2> j" . 'counsel-set-variable)
          ("C-x b" . 'ivy-switch-buffer)
          ("C-c v" . 'ivy-push-view)
          ("C-c V" . 'ivy-pop-view)

          ;; Ivy-resume and other commands
          ("C-c C-r" . 'ivy-resume)
          ("C-c b" . 'counsel-bookmark)
          ("C-c d" . 'counsel-descbinds)
          ("C-c g" . 'counsel-git)
          ("C-c o" . 'counsel-outline)
          ;; This conflicts with my typescript bindings
          ;; ("C-c t" . 'counsel-load-theme)
          ("C-c F" . 'counsel-org-file)
          ))

;; Icons plz
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :after (all-the-icons))

(use-package string-inflection
  :bind (("C-<tab>" . 'string-inflection-java-style-cycle)))

;; TIDE: TypeScript IDE mode
;; (use-package typescript-mode)
(use-package company)
(use-package web-mode
  :mode (("\\.svg\\'" . web-mode)))

;; (defmacro js-mode-list ()
;;   "Let's make a list of all the JS-ish modes for `use-package` hooks."
;;   `(typescript-mode
;;     web-mode
;;     rjsx-mode
;;     js2-mode
;;     javascript-mode))

(use-package prettier
  :hook ((typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          web-mode
          rjsx-mode
          js2-mode
          javascript-mode) . prettier-mode))

(use-package add-node-modules-path
  :init
  (eval-after-load 'markdown-mode
    '(add-hook 'markdown-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path)))

(use-package flycheck
  :hook (json-mode jsonian-mode emacs-lisp-mode markdown-mode css-mode))

;; (defmacro tide-hooks ()
;;   "Let's make a list of all the JS-ish modes for `use-package` hooks."
;;   `'((typescript-mode
;;      typescript-ts-mode
;;      tsx-ts-mode
;;      web-mode
;;      rjsx-mode
;;      js2-mode
;;      javascript-mode) . tide-setup))

;; (message (tide-hooks))

;; if you use treesitter based typescript-ts-mode (emacs 29+)
(use-package tide
  :ensure t
  :after (company flycheck)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (typescript-ts-mode . flycheck-mode)
         (tsx-ts-mode . tide-setup)
         (tsx-ts-mode . tide-hl-identifier-mode)
         (tsx-ts-mode . flycheck-mode)
         ;; (before-save . tide-format-before-save)
         )
  :bind (("C-c t n" . 'tide-rename-symbol)
         ("C-c t f" . 'tide-refactor)
         ("C-c t r" . 'tide-references)
         ("C-c t p" . 'tide-documentation-at-point)
         ("C-c t s" . 'tide-restart-server))
  :config
  (flycheck-add-mode 'typescript-tide 'typescript-ts-mode)
  (flycheck-add-mode 'typescript-tide 'tsx-ts-mode)
  (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
  (flycheck-remove-next-checker 'tsx-tide 'typescript-tslint)
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))
  (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint))
  )

;; TODO: maybe make a list of filetypes and iterate over it here, and below.
;; (defun tide-file-init-is-js (extension)
;;   "Test whether a file EXTENSION implies a Javascript-like language."
;;   (or (string= "tsx" extension)
;;     (string= "jsx" extension)
;;     (string= "ts" extension)
;;     (string= "js" extension)
;;     (string= "mjs" extension)))

;; (defun tide-file-init ()
;;   "Do everything necessary when we go into typescript-mode or web-mode."
;;   (when (tide-file-init-is-js (file-name-extension buffer-file-name))
;;     (message "hey, this is tide-file-init")
;;     (tide-setup)
;;     (tide-hl-identifier-mode)
;;     (add-node-modules-path)
;;     (flycheck-mode)))

;; (use-package tide
;;   :after (typescript-mode company flycheck web-mode add-node-modules-path)
;;   :mode (("\\.ts\\'" . typescript-mode)
;;           ("\\.js\\'" . typescript-mode)
;;           ("\\.jsx\\'" . web-mode)
;;           ("\\.tsx\\'" . web-mode)
;;           ("\\.mjs\\'" . typescript-mode))
;;   :hook ((typescript-mode . tide-file-init)
;;           (javascript-mode . tide-file-init)
;;           (web-mode . tide-file-init))
;;   :bind (("C-c t n" . 'tide-rename-symbol)
;;           ("C-c t f" . 'tide-refactor)
;;           ("C-c t r" . 'tide-references)
;;           ("C-c t p" . 'tide-documentation-at-point)
;;           ("C-c t s" . 'tide-restart-server))
;;   :config
;;   ;; To make eslint work in .tsx files
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)

;;   ;; For TS, I don't want tslint and do want eslint
;;   (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
;;   (flycheck-remove-next-checker 'tsx-tide 'typescript-tslint)
;;   (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))
;;   (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint)))

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

;; load up rainbow-mode in various modes in which we may find it useful
(use-package rainbow-mode
  :hook (javascript-mode typescript-mode css-mode vue-mode web-mode))

;; projectile
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; also disable interlock symlinks
(setq create-lockfiles nil)

;; expand region binding
(global-set-key (kbd "C-=") 'er/expand-region)

;; yasnippet setup
;; Bind yas-expand to shift-tab
(use-package yasnippet
  :config
  (global-set-key (kbd "<S-tab>") 'yas-expand)
  (yas-global-mode 1))

;; a function for reinstalling selected packages
(defun package-reinstall-activated ()
  "Reinstall all activated packages."
  (interactive)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name)
                (warn "Package %s failed to reinstall" package-name))))))

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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; Dired
;; Add icons to dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; Nicollet tools
(load-file "~/.emacs.d/geoff/nicollet.el")

(defun jsx ()
  "Set the right modes for JSX in .js files."
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx"))


;; Org-mode config
;; (add-hook
;;  'org-mode-hook
;;  #'(lambda ()
;;      (org-indent-mode)))


;; Org-Jira
;; (use-package org-jira
;;   :config
;;   (setq jiralib-url "https://jira.target.com"))

;; eshell-toggle
(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term)
  ;; :quelpa
  ;; (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  ("C-`" . eshell-toggle))

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; Python IDE
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package glsl-mode)

;; This makes GhostEdit (Firefox) work -- for editing text in browser from Emacs
(use-package atomic-chrome
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("shadertoy\\.com" . glsl-mode)))
  :init
  (atomic-chrome-start-server))

(use-package polymode)

;; (use-package tree-sitter
;;   :init
;;   (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs)

;; Built-in tree sitter config
(setq treesit-extra-load-path '("~/.emacs.d/treesit"))

(use-package markdown-mode)

(use-package jsonian
  :after flycheck
  :init (jsonian-enable-flycheck)
  :mode ("\\.json\\'" . jsonian-mode))

;; To start the server on MacOS:
;; 
;;   brew services start languagetool
;; 
;; See also https://dev.languagetool.org/http-server
(use-package languagetool)

(use-package presentation)

;; (use-package edit-server
;;   :ensure t
;;   :commands edit-server-start
;;   :init (if after-init-time
;;             (edit-server-start)
;;           (add-hook 'after-init-hook
;;                     #'(lambda() (edit-server-start))))
;;   :config (setq edit-server-new-frame-alist
;;                 '((name . "Edit with Emacs FRAME")
;;                   (top . 200)
;;                   (left . 200)
;;                   (width . 80)
;;                   (height . 25)
;;                   (minibuffer . t)
;;                   (menu-bar-lines . t)
;;                   (window-system . x))))

;; unbind M-o from HTML mode
(defun html-mode-setup ()
  "Customize keybindings for HTML mode."
  (defvar html-mode-map)
  (unbind-key "M-o" html-mode-map))
(add-hook 'html-mode-hook 'html-mode-setup)

(defun edit-init ()
  "Edit the init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun edit-tide-config ()
  "Edit the tide config in init.el."
  (interactive)
  (edit-init)
  (search-forward "(use-package tide")
  (recenter-top-bottom 2))

;; Bring in my own packages
(require 'slugify "~/.emacs.d/geoff/slugify.el")

(put 'magit-clean 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(provide 'init)
;;; init.el ends here
