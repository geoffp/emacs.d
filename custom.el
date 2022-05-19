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
 '(auth-source-save-behavior nil)
 '(bookmark-save-flag 1)
 '(compilation-message-face 'default)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
    '("3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(drag-stuff-global-mode t)
 '(editorconfig-mode t)
 '(elpy-rpc-python-command "python3")
 '(fill-column 80)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-disabled-checkers '(javascript-standard))
 '(flycheck-idle-change-delay 1)
 '(global-auto-revert-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
    (--map
      (solarized-color-blend it "#002b36" 0.25)
      '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(hl-bg-colors
    '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
    '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
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
 '(neo-theme 'icons)
 '(nrepl-message-colors
    '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-command-modifier 'meta)
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-export-headline-levels 2)
 '(org-export-with-section-numbers nil)
 '(package-enable-at-startup nil)
 '(package-selected-packages
    '(string-inflection prettier-mode flycheck prettier flycheck-mode prettier-js-mode auto-package-update elpy kotlin-mode eshell-toggle org-jira counsel-css counsel-projectile counsel restclient dockerfile-mode smart-mode-line swift-mode edit-indirect exec-path-from-shell yasnippet projectile-ripgrep use-package presentation polymode magit doom-themes tide all-the-icons all-the-icons-dired company projectile visual-regexp web-mode with-editor pcre2el visual-regexp-steroids ace-window nginx-mode yaml-mode typescript-mode gh-md emojify add-node-modules-path iedit rainbow-mode expand-region js-doc auto-complete ag json-mode markdown-mode editorconfig neotree))
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-width 2)
 '(tide-native-json-parsing t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(uniquify-strip-common-suffix nil)
 '(vc-annotate-background-mode nil)
 '(warning-suppress-types '((comp)))
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
 '(tide-hl-identifier-face ((t (:inherit region))))
 '(web-mode-interpolate-color1-face ((t (:inherit web-mode-css-property-name-face))))
 '(web-mode-interpolate-color2-face ((t (:inherit default)))))
