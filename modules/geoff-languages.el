;;; geoff-languages.el --- Programming language support  -*- lexical-binding: t; -*-

;;; Commentary:
;; Eglot (LSP), tree-sitter modes, and code formatting (prettier/biome).

;;; Code:

;; --- Eglot (built-in LSP client) ---
(require 'eglot)

(add-to-list 'eglot-server-programs
  '(toml-ts-mode "taplo" "lsp" "stdio"))
(add-to-list 'eglot-server-programs
  '(((web-mode :language-id "javascript"))
    "typescript-language-server" "--stdio"))
(add-to-list 'eglot-server-programs
  '(((css-mode :language-id "css"))
    "vscode-css-language-server" "--stdio"))

;; Blocklist diagnostics in modes where LSP diagnostics don't work well
(defcustom geoff-eglot-diagnostics-blocklist
  '(css-mode css-ts-mode scss-ts-mode)
  "Modes where eglot's flymake integration should be disabled."
  :group 'eglot
  :type '(repeat symbol))

(add-hook 'eglot-managed-mode-hook
  (lambda ()
    (when (memq major-mode geoff-eglot-diagnostics-blocklist)
      (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t))))

;; --- TypeScript / TSX ---
(use-package typescript-ts-mode
  :ensure nil ; built-in
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . my/set-typescript-tsdk)
         (tsx-ts-mode . my/set-typescript-tsdk))
  :init
  (defun my/set-typescript-tsdk ()
    "Point eglot at the project-local TypeScript."
    (when-let* ((root (locate-dominating-file default-directory "node_modules"))
                (tsdk (expand-file-name "node_modules/typescript/lib" root)))
      (when (file-directory-p tsdk)
        (setq-local eglot-workspace-configuration
                    `(:typescript (:tsdk ,tsdk)))))))

;; --- Go ---
(use-package go-ts-mode
  :ensure nil ; built-in
  :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . eglot-ensure)
  :init
  (require 'treesit)
  (add-to-list 'treesit-language-source-alist
    '(gomod "https://github.com/camdencheek/tree-sitter-go-mod"))
  (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode)))

;; --- JavaScript ---
(use-package js-ts-mode
  :ensure nil ; built-in
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . eglot-ensure))

;; --- CSS / SCSS ---
(use-package css-ts-mode
  :ensure nil ; built-in
  :mode (("\\.css\\'" . css-ts-mode)
         ("\\.scss\\'" . scss-ts-mode))
  :hook ((css-ts-mode . eglot-ensure)
         (scss-ts-mode . eglot-ensure)))

;; --- Web / HTML / templates ---
(use-package web-mode
  :mode ("\\.hbs\\'" "\\.html\\'" "\\.mustache\\'"
         "\\.webc\\'" "\\.liquid\\'" "\\.eta\\'"))

(use-package pug-mode
  :mode "\\.pug\\'")

;; --- JSON / YAML (built-in tree-sitter modes) ---
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))

;; --- JSON syntax checking (flymake, no external tools needed) ---
(load (expand-file-name "json-simple-flymake.el" (expand-file-name "geoff" user-emacs-directory)))
(add-hook 'json-ts-mode-hook
  (lambda ()
    (json-simple-setup-flymake-backend)
    (flymake-mode)))

;; --- Code formatting: prettier / biome ---
;; Auto-detects which formatter to use based on project config files.
(use-package prettier
  :defer t
  :init (setq prettier-js-use-modules-bin t))

(use-package biomejs-format
  :defer t)

(require 'js-code-formatting)

;; --- Unbind M-. from js-mode (let xref-find-definitions win) ---
(add-hook 'js-mode-hook
  (lambda () (unbind-key "M-." js-mode-map)))
(add-hook 'js-ts-mode-hook
  (lambda () (unbind-key "M-." js-ts-mode-map)))

;; --- XML formatting ---
(defun reformat-xml ()
  "Pretty-print an XML document using xmllint."
  (interactive)
  (call-process-region (point-min) (point-max) "xmllint" t t t "--format" "-")
  (goto-char (point-min)))

;; --- Org ---
(use-package org-modern)

(provide 'geoff-languages)
;;; geoff-languages.el ends here
