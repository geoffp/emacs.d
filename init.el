;;; init --- my .emacs.d/init.el
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; (package-initialize)

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

;; keep packages up to date
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; start requiring packages we need

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package magit
  :bind (("C-M-g" . 'magit-status)))

(use-package ace-window
  :bind (("M-o" . 'ace-window)))

(use-package counsel)

(use-package iedit)

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
(use-package all-the-icons)
(use-package all-the-icons-dired
  :after (all-the-icons))

(use-package string-inflection
  :bind (("C-<tab>" . 'string-inflection-java-style-cycle)))

;; TIDE: TypeScript IDE mode
(use-package typescript-mode)
(use-package company)
(use-package web-mode)

(defmacro js-mode-list ()
  "Let's make a list of all the JS-ish modes for use-package hooks."
  `(typescript-mode
    web-mode
    rjsx-mode
    js2-mode
    javascript-mode))

(use-package prettier
  :hook ((typescript-mode
         web-mode
         rjsx-mode
         js2-mode
           javascript-mode) . prettier-mode))

(use-package flycheck
  :hook ((json-mode emacs-lisp-mode) . flycheck-mode))

(use-package add-node-modules-path)

;; TODO: maybe make a list of filetypes and iterate over it here, and below.
(defun tide-file-init-is-js (extension)
  "Test whether a file EXTENSION implies a Javascript-like language."
  (or (string= "tsx" extension)
    (string= "jsx" extension)
    (string= "ts" extension)
    (string= "js" extension)
    (string= "mjs" extension)))

(defun tide-file-init ()
  "Do everything necessary when we go into typescript-mode or web-mode."
  (when (tide-file-init-is-js (file-name-extension buffer-file-name))
    (message "hey, this is tide-file-init")
    (tide-setup)
    (tide-hl-identifier-mode)
    (add-node-modules-path)
    (flycheck-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck web-mode add-node-modules-path)
  :mode (("\\.ts\\'" . typescript-mode)
          ("\\.js\\'" . typescript-mode)
          ("\\.jsx\\'" . web-mode)
          ("\\.tsx\\'" . web-mode)
          ("\\.mjs\\'" . typescript-mode))
  :hook ((typescript-mode . tide-file-init)
          (javascript-mode . tide-file-init)
          (web-mode . tide-file-init))
  :bind (("C-c t n" . 'tide-rename-symbol)
          ("C-c t f" . 'tide-refactor)
          ("C-c t r" . 'tide-references)
          ("C-c t p" . 'tide-documentation-at-point)
          ("C-c t s" . 'tide-restart-server))
  :config
  ;; To make eslint work in .tsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; For TS, I don't want tslint and do want eslint
  (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
  (flycheck-remove-next-checker 'tsx-tide 'typescript-tslint)
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint))
  (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint)))

(use-package visual-regexp)

;;
;; company mode, for auto-completion wherever possible.
;;
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;; global emojify: no thanks
;; (add-hook 'after-init-hook #'global-emojify-mode)
;; (require 'company-emoji)
;; (add-to-list 'company-backends 'company-emoji)
(global-set-key (kbd "M-RET") 'company-complete)

;; file types to open in web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))


;; load up rainbow-mode in various modes in which we may find it useful
(use-package rainbow-mode
  :hook (javascript-mode typescript-mode css-mode vue-mode web-mode))
;; (add-hook 'javascript-mode-hook `rainbow-mode)
;; (add-hook 'css-mode-hook `rainbow-mode)
;; (add-hook 'vue-mode-hook `rainbow-mode)
;; (add-hook 'web-mode-hook `rainbow-mode)
;; (add-hook 'typescript-mode-hook `rainbow-mode)

;; load node_modules into the exec path when we open certain things in a buffer
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))
;; (eval-after-load 'js2-mode
;;   '(add-hook 'js2-mode-hook #'add-node-modules-path))
(eval-after-load 'css-mode
  '(add-hook 'css-mode-hook #'add-node-modules-path))
(eval-after-load 'scss-mode
  '(add-hook 'scss-mode-hook #'add-node-modules-path))
;; (eval-after-load 'rjsx-mode
;;   '(add-hook 'rjsx-mode-hook #'add-node-modules-path))
(eval-after-load 'vue-mode
  '(add-hook 'vue-mode-hook #'add-node-modules-path))
(eval-after-load 'web-mode
  '(add-hook 'vue-mode-hook #'add-node-modules-path))
(eval-after-load 'typescript-mode
  '(add-hook 'typescript-mode-hook #'add-node-modules-path))

;; projectile
(use-package projectile
  :ensure t
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
(load-file "~/.emacs.d/nicollet.el")

(defun jsx ()
  "Set the right modes for JSX in .js files."
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx"))

;; Org-Jira
(use-package org-jira
  :config
  (setq jiralib-url "https://jira.target.com"))

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
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package glsl-mode
  :ensure t)

;; (use-package edit-server
;;   :ensure t
;;   :init
;;   (setq edit-server-new-frame nil)
;;   (edit-server-start))

(use-package atomic-chrome
  :ensure t
  :custom
  (atomic-chrome-url-major-mode-alist
   '(("shadertoy\\.com" . glsl-mode)))
  :init
  (atomic-chrome-start-server))


(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
