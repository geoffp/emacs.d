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

;; (use-package eglot-booster
;;   :straight '(eglot-booster :type git :host github
;;               :repo "jdtsmith/eglot-booster")
;; 	:after eglot
;; 	:config	(eglot-booster-mode))

(provide 'eglot-config)
;;; eglot-config.el ends here
