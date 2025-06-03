;;; init --- my lsp-mode config
;;; Commentary:
;;; Code:

;; Configure stuff for lsp-mode, js, ts, etc.
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
          (css-mode . lsp)
          (css-ts-mode . lsp)
          (js-ts-mode . lsp)
          (typescript-ts-mode . lsp)
          (tsx-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp)

;; ;; optionally
;; (use-package consult-lsp :commands lsp-ui-mode)
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; (use-package polymode
;;   :config
;;   (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'lsp-on-change)
;;   (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'lsp-before-change))

;; The path to lsp-mode needs to be added to load-path as well as the
;; path to the `clients' subdirectory.
(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))

(provide 'lsp-mode-config)
;;; lsp-mode-config.el ends here
