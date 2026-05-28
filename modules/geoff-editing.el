;;; geoff-editing.el --- Editing enhancements  -*- lexical-binding: t; -*-

;;; Commentary:
;; General editing tools: snippets, multi-edit, search/replace, etc.

;;; Code:

;; --- Snippets ---
(use-package yasnippet
  :config
  (global-set-key (kbd "<S-tab>") #'yas-expand)
  (yas-global-mode 1))

;; --- Multi-cursor editing ---
(use-package iedit)

;; --- Editable grep buffers ---
(use-package wgrep)

;; --- Multi-file occur ---
(use-package noccur)

;; --- String case cycling ---
(use-package string-inflection
  :bind ("C-<tab>" . string-inflection-java-style-cycle))

;; --- EditorConfig (built-in to Emacs 30) ---
(editorconfig-mode 1)

;; --- Rainbow colors in CSS etc. ---
(use-package rainbow-mode
  :hook (css-mode css-ts-mode web-mode))

;; --- Markdown ---
(use-package markdown-mode)

;; --- Presentation mode ---
(use-package presentation)

;; --- Edit regions in separate buffers ---
(use-package edit-indirect)

;; --- Eshell ---
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

(provide 'geoff-editing)
;;; geoff-editing.el ends here
