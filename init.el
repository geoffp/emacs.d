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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ag-ignore-list
     '("node_modules" "*.map" "main.*.*" "*.snap" "*.min.js" "lib" "es" "vendorAssets" "dist" "dist-es2015"))
  '(ansi-color-faces-vector
     [default bold shadow italic underline bold bold-italic bold])
 '(async-bytecomp-package-mode t)
 '(bookmark-save-flag 1)
 '(compilation-message-face 'default)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(doom-one))
  '(custom-safe-themes
     '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(drag-stuff-global-mode t)
 '(editorconfig-mode t)
 '(fill-column 80)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-disabled-checkers '(javascript-standard))
 '(flycheck-idle-change-delay 1)
 '(global-auto-complete-mode t)
 '(global-auto-revert-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
  '(highlight-symbol-colors
     (--map
       (solarized-color-blend it "#002b36" 0.25)
       '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
  '(highlight-tail-colors
     '(("#073642" . 0)
        ("#546E00" . 20)
        ("#00736F" . 30)
        ("#00629D" . 50)
        ("#7B6000" . 60)
        ("#8B2C02" . 70)
        ("#93115C" . 85)
        ("#073642" . 100)))
  '(hl-bg-colors
     '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
  '(hl-fg-colors
     '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(ido-mode 'both nil (ido))
 '(indent-tabs-mode nil)
 '(js-chain-indent t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2-include-node-externs t)
 '(js2-skip-preprocessor-directives t)
 '(js2-strict-missing-semi-warning nil)
 '(js2-strict-trailing-comma-warning nil)
 '(magit-auto-revert-mode t)
 '(magit-diff-use-overlays nil)
 '(markdown-command "pandoc")
 '(mocha-options "--recursive --colors")
 '(mocha-reporter "spec")
  '(nrepl-message-colors
     '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-command-modifier 'meta)
 '(package-enable-at-startup nil)
  '(package-selected-packages
     '(smart-mode-line swift-mode edit-indirect exec-path-from-shell yasnippet projectile-ripgrep use-package presentation polymode magit doom-themes tide all-the-icons all-the-icons-dired company projectile visual-regexp web-mode with-editor pcre2el visual-regexp-steroids ace-window nginx-mode prettier-js yaml-mode typescript-mode gh-md emojify add-node-modules-path drag-stuff iedit rainbow-mode expand-region js-doc auto-complete ag json-mode flycheck markdown-mode editorconfig flx-ido neotree))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(presentation-default-text-scale 2)
  '(projectile-globally-ignored-directories
     '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "lib" "es" "build"))
 '(projectile-globally-ignored-file-suffixes '(".map"))
  '(rainbow-html-colors-major-mode-list
     '(html-mode css-mode php-mode nxml-mode xml-mode web-mode js2-mode rjsx-mode))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-width 2)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tide-native-json-parsing t)
 '(tool-bar-mode nil)
 '(vc-annotate-background-mode nil)
  '(weechat-color-list
     '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
  '(xterm-color-names
     ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
  '(xterm-color-names-bright
     ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(setq default-directory "~/src/nicollet")

;; window navigation keybindings
(global-set-key (kbd "M-o") 'ace-window)

;; drag-stuff default keybindings
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(use-package magit)

;; prettier-js-mode by default for JS
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'javascript-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode)

;; TIDE: TypeScript IDE mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  ;; Use eslint also
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  ;; set keyboard shortcuts
  )

(use-package typescript-mode)
(use-package company)
(use-package web-mode)
(use-package flycheck
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; TODO: maybe make a list of filetypes and iterate over it here, and below.
(defun tide-file-init-is-js (extension)
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
          ("C-c t p" . 'tide-documentation-at-point))
  :config
  (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
  (flycheck-remove-next-checker 'tsx-tide 'typescript-tslint)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append))


;; Web-mode + TIDE for .tsx
;; (require 'web-mode)
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; enable typescript-tslint checker


;; Use Web-mode + TIDE for .jsx also
;; DEPRECATED: using use-package now.
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

;; configure jsx-tide checker to run after your default jsx checker
;; DEPRECATED: using use-package now.
;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

;; TypeScript mode by default
;; DEPRECATED: using use-package now.
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-mode))

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
(let ((f "~/.emacs.local"))
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

(load-file "~/src/nicollet-emacs-tools/nicollet-changelogs.el")

;; Dired
;; Add icons to dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)



(provide 'init)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
(put 'magit-clean 'disabled nil)
