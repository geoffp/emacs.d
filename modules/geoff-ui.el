;;; geoff-ui.el --- Visual appearance  -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme, icons, ligatures, and display settings.

;;; Code:

;; --- Theme ---
(use-package zenburn-theme
  :config (load-theme 'zenburn t))

;; --- Icons (nerd-icons: single package, fast) ---
;; nerd-icons defaults to "Symbols Nerd Font Mono" for glyphs, which is a
;; separate font you'd need to install.  Since our main font (FiraCode Nerd
;; Font Mono) already includes all the Nerd Font glyphs, we just point
;; nerd-icons at it directly.
(use-package nerd-icons
  :if (display-graphic-p)
  :custom
  (nerd-icons-font-family "FiraCode Nerd Font Mono"))

(use-package nerd-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (display-graphic-p)
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init (nerd-icons-completion-mode))

;; --- Ligatures ---
(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode
    '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
      "\\\\" "://"))
  (global-ligature-mode t))

;; --- Dired ---
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; --- Misc display ---
(setq system-uses-terminfo nil)

(provide 'geoff-ui)
;;; geoff-ui.el ends here
