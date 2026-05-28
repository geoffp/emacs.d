;;; geoff-tools.el --- External tools integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Git, terminal, REST, shell environment, MCP server.

;;; Code:

;; --- Shell environment (macOS GUI Emacs doesn't inherit shell env) ---
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;; --- Git ---
(use-package magit
  :bind ("C-M-g" . magit-status))

;; --- Terminal ---
(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t))

;; --- REST client ---
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

;; --- MCP server ---
(use-package mcp-server
  :load-path "~/src/emacs-mcp-server"
  :config
  (add-hook 'emacs-startup-hook #'mcp-server-start-unix))

;; --- Unbind M-o in term-raw-mode ---
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "M-o") nil))

(provide 'geoff-tools)
;;; geoff-tools.el ends here
