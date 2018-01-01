;;; init --- my .emacs.d/init.el
;;; Commentary:
;;; Code:
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)

(exec-path-from-shell-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list (quote ("node_modules" "*.map" "main.*.*")))
  '(ansi-color-faces-vector
     [default bold shadow italic underline bold bold-italic bold])
 '(async-bytecomp-package-mode t)
 '(bookmark-save-flag 1)
 '(css-indent-offset 2)
 '(custom-enabled-themes (quote (solarized-dark)))
  '(custom-safe-themes
     (quote
       ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(editorconfig-mode t)
 '(fill-column 95)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-disabled-checkers (quote (javascript-standard)))
 '(flycheck-idle-change-delay 1)
 '(global-auto-complete-mode t)
 '(global-flycheck-mode t)
 '(ido-mode (quote both) nil (ido))
 '(js-indent-level 2)
 '(js2-strict-missing-semi-warning nil)
 '(mocha-options "--recursive --colors")
 '(mocha-reporter "spec")
 '(ns-command-modifier (quote meta))
 '(package-enable-at-startup nil)
 '(package-selected-packages
	 (quote
		(prettier-js company-tern drag-stuff vue-mode iedit mocha mmm-mode nginx-mode swift-mode rjsx-mode yaml-mode yasnippet rainbow-mode expand-region js-doc auto-complete web-mode ag json-mode exec-path-from-shell flycheck markdown-mode jade editorconfig magit flx-ido projectile neotree js2-mode js2-refactor gruvbox-theme monokai-theme zenburn-theme spacegray-theme solarized-theme color-theme-solarized color-theme-sanityinc-tomorrow)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
  '(rainbow-html-colors-major-mode-list
     (quote
       (html-mode css-mode php-mode nxml-mode xml-mode web-mode js2-mode rjsx-mode)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tab-width 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 120 :width normal))))
 '(mmm-default-submode-face ((t nil))))

;; add melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; window navigation keybindings
;; (global-set-key (kbd "M-<up>") 'windmove-up)
;; (global-set-key (kbd "M-<down>") 'windmove-down)
;; (global-set-key (kbd "M-<right>") 'windmove-right)
;; (global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-o") 'other-window)

;; drag-stuff default keybindings
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; js2 mode by default
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; company mode, for auto-completion wherever possible.
;; for javascript, this means you have to:
;;   npm install -g tern
;; tern provides the code analysis and type inference, so you must also enable tern-mode for JS
;; buffers
(require 'company)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-RET") 'company-complete)
;; js modes
(add-hook 'rjsx-mode-hook 'tern-mode)
(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'javascript-mode-hook 'tern-mode)

;; file types to open in web-mode
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

;; make .html underscore template work in web-mode
(setq web-mode-content-types-alist
  '(("underscore.js" . "\\.html\\'")
     ("jsx"          . "\\.js[x]?\\'")))

;; js2-refactor-mode by default
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-m")


;; load up rainbow-mode in various modes in which we may find it useful
(add-hook 'js2-mode-hook `rainbow-mode)
(add-hook 'rjsx-mode-hook `rainbow-mode)
(add-hook 'javascript-mode-hook `rainbow-mode)
(add-hook 'css-mode-hook `rainbow-mode)
(add-hook 'vue-mode-hook `rainbow-mode)

;; ido setup
(flx-ido-mode)

;; projectile
(projectile-mode)

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
(let ((f "~/.emacs.local"))
  (if (file-readable-p f)
      (load-file f)))


;; let's do flycheck for JS in web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; as long we're talking flycheck, let's teach it to find eslint in node_modules.
;; ripped from https://emacs.stackexchange.com/a/21207
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; load up mmm-mode
(require 'mmm-mode)


(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
