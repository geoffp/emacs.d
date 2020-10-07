;;; init --- my .emacs.d/init.el
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; (package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
;; TODO: does this actually work?
;; (cd "~/src/nicollet/")
(setq default-directory "~/src/")
(setq command-line-default-directory "~/src/")

;; window navigation keybindings
(global-set-key (kbd "M-o") 'ace-window)

;; drag-stuff default keybindings
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(global-auto-revert-mode)

(use-package all-the-icons)

(use-package magit)

;; prettier-js-mode by default for JS
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'javascript-mode-hook 'prettier-js-mode)

;; Let's make a list of all the JS-ish modes for use-package hooks


;; TIDE: TypeScript IDE mode
(use-package typescript-mode)
(use-package company)
(use-package web-mode)
;; (use-package rjsx-mode)
;; (use-package js2-mode)

(defmacro js-mode-list ()
  `(typescript-mode
    web-mode
    rjsx-mode
    js2-mode
    javascript-mode))

(use-package prettier-js-mode
  :hook (typescript-mode
         web-mode
         rjsx-mode
         js2-mode
         javascript-mode))

(use-package flycheck-mode
  :hook (typescript-mode
         web-mode
         rjsx-mode
         js2-mode
         javascript-mode)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))



;; TODO: maybe make a list of filetypes and iterate over it here, and below.
(defun tide-file-init-is-js (extension)
  "Test whether a file extension implies a Javascript-like language"
  (or (string= "tsx" extension)
    (string= "jsx" extension)
    (string= "ts" extension)
    (string= "js" extension)
    (string= "mjs" extension)))

(defun tide-file-init ()
  "Do everything necessary when we go into typescript-mode or web-mode"
  (when (tide-file-init-is-js (file-name-extension buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck web-mode)
  :mode (("\\.ts\\'" . typescript-mode)
          ("\\.js\\'" . typescript-mode)
          ("\\.jsx\\'" . web-mode)
          ("\\.tsx\\'" . web-mode)
          ("\\.mjs\\'" . typescript-mode))
  :hook ((typescript-mode . tide-file-init)
          (web-mode . tide-file-init))
  :bind (("C-c t n" . 'tide-rename-symbol)
          ("C-c t f" . 'tide-refactor)
          ("C-c t r" . 'tide-references)
          ("C-c t p" . 'tide-documentation-at-point)
          ("C-c t s" . 'tide-restart-server))
  :config
  (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
  (flycheck-remove-next-checker 'tsx-tide 'typescript-tslint)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append))

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

(defun web-jsx ()
  "Set a JS file to web mode, with JSX content."
  (interactive)
  (web-mode)
  (web-mode-set-content-type "jsx"))

;; make .html underscore template work in web-mode
;; (setq web-mode-content-types-alist
;;   '(("underscore.js" . "\\.html\\'")
;;      ("jsx"          . "\\.js[x]?\\'")))

;; js2-refactor-mode by default
;; (add-hook 'js2-mode-hook #'js2-refactor-mode)
;; (js2r-add-keybindings-with-prefix "C-c C-m")


;; load up rainbow-mode in various modes in which we may find it useful
;; (add-hook 'js2-mode-hook `rainbow-mode)
;; (add-hook 'rjsx-mode-hook `rainbow-mode)
(add-hook 'javascript-mode-hook `rainbow-mode)
(add-hook 'css-mode-hook `rainbow-mode)
(add-hook 'vue-mode-hook `rainbow-mode)
(add-hook 'web-mode-hook `rainbow-mode)
(add-hook 'typescript-mode-hook `rainbow-mode)

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

;; manually add this one until https://github.com/codesuki/add-node-modules-path/issues/8 is resolved
(add-to-list 'exec-path "~/src/nicollet/node_modules/.bin")

;; ido setup
(flx-ido-mode)

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

;; Magit status key binding
(global-set-key (kbd "C-M-g") 'magit-status)

;; yasnippet setup
;; Bind yas-expand to shift-tab
(global-set-key (kbd "<S-tab>") 'yas-expand)
(yas-global-mode 1)

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


;; as long we're talking flycheck, let's teach it to find eslint in node_modules.
;; ripped from https://emacs.stackexchange.com/a/21207
;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; pretty printing commands
(defun reformat-xml ()
  "Pretty-print an XML document using xmllint."
  (interactive)
  (call-process-region (point-min) (point-max) "xmllint" t t t "--format" "-")
  (goto-char (point-min)))

(reformat-xml)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (load-file "~/src/nicollet-emacs-tools/nicollet-changelogs.el")

;; Dired
;; Add icons to dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)



(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
