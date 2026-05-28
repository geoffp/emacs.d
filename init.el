;;; init.el --- Bootstrap  -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimal bootstrap: package setup, paths, then load modules.

;;; Code:

;; --- Package setup ---
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; --- Load path ---
(add-to-list 'load-path (expand-file-name "geoff" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; --- Zscaler bypass (before package-initialize fetches anything) ---
(require 'url-direct)
(url-direct-maybe-enable)

(package-initialize)

;; --- Custom file ---
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; --- Default working directory ---
(let ((default-dir "~/src/uxe/"))
  (when (file-directory-p default-dir)
    (setq default-directory default-dir)
    (setq command-line-default-directory default-dir)))

;; --- Backups & lockfiles ---
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;; --- Modules ---
(require 'geoff-completion)
(require 'geoff-ui)
(require 'geoff-editing)
(require 'geoff-languages)
(require 'geoff-tools)

;; --- Personal libraries ---
(require 'slugify)
(require 'misc)

;; --- Local overrides (machine-specific, not in git) ---
(let ((f (expand-file-name "init.local.el" user-emacs-directory)))
  (when (file-readable-p f)
    (load-file f)))

;; --- Global keybindings ---
(bind-key "M-o" #'other-window)
(bind-key "C-h SPC" #'help-follow-symbol)

;; --- Aliases ---
(defalias 'font-window 'menu-set-font)

(provide 'init)
;;; init.el ends here
