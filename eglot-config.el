;;; init --- my eglot config
;;; Commentary:
;;; Code:

;; Configure stuff for eglot, js, ts, etc.
(require 'eglot)
(add-to-list 'eglot-server-programs '(toml-ts-mode "taplo" "lsp" "stdio"))
(add-to-list 'eglot-server-programs '(((web-mode :language-id "javascript"))
                                      "typescript-language-server" "--stdio"))
(add-to-list 'eglot-server-programs '(((css-mode :language-id "css"))
                                       "vscode-css-language-server" "--stdio"))

;; Define customize variable to hold the list of modes whose LSP implementations have non-standard diagnostics functionality that eglot doesn't support
(defcustom eglot-blocklisted-diagnostics-modes
  '("css-mode" "css-ts-mode" "json-mode" "json-ts-mode")
  "The modes in which to automatically enable JS code formatting."
  :group 'eglot
  :type '(repeat string))

;; Don't enable flymake diagnostics hooks in modes in which they don't work due to non-standard LSP implementations
(add-hook 'eglot-managed-mode-hook
  (lambda ()
    (let* ((is-blocklisted (member (symbol-name major-mode) eglot-blocklisted-diagnostics-modes)))
      (if is-blocklisted
        (progn
          (message (format "eglot diagnostics blocklisted in %s" major-mode))
          (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend))))))

;; (use-package eglot-booster
;;   :straight '(eglot-booster :type git :host github
;;               :repo "jdtsmith/eglot-booster")
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

(provide 'eglot-config)
;;; eglot-config.el ends here
