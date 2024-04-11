;;; custom --- all things customized by Custom go in here
;;; Commentary:
;;; yay
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-ignore-list
   '("node_modules" "*.map" "main.*.*" "*.snap" "*.min.js" "lib" "es" "vendorAssets" "dist" "dist-es2015"))
 '(all-the-icons-dired-monochrome nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(async-bytecomp-package-mode t)
 '(auth-source-save-behavior nil)
 '(auth-sources
   '(macos-keychain-internet macos-keychain-generic "~/.authinfo.gpg"))
 '(bookmark-save-flag 1)
 '(byte-compile-warnings nil)
 '(compilation-message-face 'default)
 '(connection-local-criteria-alist
   '(((:application tramp :machine "MJ2X1FC4CR")
      tramp-connection-local-darwin-ps-profile)
     ((:machine "usertestingproxy-proxy")
      usertestingproxy-proxy-vars)
     ((:application eshell)
      eshell-connection-default-profile)
     ((:machine "basementcat")
      basementcat-vars)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "C02ZW65MMD6R")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((usertestingproxy-proxy-vars
      (company-gtags--executable-connection))
     (eshell-connection-default-profile
      (eshell-path-env-list))
     (basementcat-vars
      (company-gtags--executable-connection))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(counsel-projectile-mode t)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "1436985fac77baf06193993d88fa7d6b358ad7d600c1e52d12e64a2f07f07176" "3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "77113617a0642d74767295c4408e17da3bfd9aa80aaa2b4eeb34680f6172d71a" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "54f2d1fcc9bcadedd50398697618f7c34aceb9966a6cbaa99829eb64c0c1f3ca" "04232a0bfc50eac64c12471607090ecac9d7fd2d79e388f8543d1c5439ed81f5" "d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "a800120841da457aa2f86b98fb9fd8df8ba682cebde033d7dbf8077c1b7d677a" "3fd0fda6c3842e59f3a307d01f105cce74e1981c6670bb17588557b4cebfe1a7" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(dired-listing-switches "-alh")
 '(drag-stuff-global-mode t)
 '(editorconfig-mode t)
 '(elpy-rpc-python-command "python3")
 '(fill-column 80)
 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-disabled-checkers '(javascript-standard))
 '(flycheck-idle-change-delay 1)
 '(global-auto-revert-mode t)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.gz"))
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
 '(languagetool-console-command "/usr/local/bin/languagetool")
 '(lsp-ui-doc-delay 1)
 '(lsp-ui-doc-show-with-cursor t)
 '(magit-auto-revert-mode t)
 '(magit-diff-use-overlays nil)
 '(markdown-command "pandoc")
 '(mocha-options "--recursive --colors")
 '(mocha-reporter "spec")
 '(neo-theme 'icons)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(ns-command-modifier 'meta)
 '(org-cycle-separator-lines 2)
 '(org-export-headline-levels 2)
 '(org-export-with-section-numbers nil)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   '(elpy notmuch-addr ztree add-node-modules-path urlenc jinja2-mode slack editorconfig markdown-mode counsel magit wgrep jsonian-mode jsonian org-mode svgo languagetool ts-fold use-package html-mode ace-window ag all-the-icons all-the-icons-dired atomic-chrome auto-complete auto-package-update company counsel-css counsel-projectile dockerfile-mode doom-themes dracula-theme edit-indirect edit-server emojify eshell-toggle exec-path-from-shell expand-region flycheck flycheck-mode gh-md glsl-mode iedit js-doc json-mode kotlin-mode lispy neotree nginx-mode pcre2el polymode presentation prettier prettier-js-mode prettier-mode projectile projectile-ripgrep rainbow-mode restclient smart-mode-line string-inflection swift-mode tide use-packages visual-regexp visual-regexp-steroids web-mode with-editor yaml-mode yasnippet zenburn-theme))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(presentation-default-text-scale 2)
 '(project-vc-ignores '("./docs/"))
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "lib" "es" "build" "docs" "dist"))
 '(projectile-globally-ignored-file-suffixes '(".map"))
 '(rainbow-html-colors-major-mode-list
   '(html-mode css-mode php-mode nxml-mode xml-mode web-mode js2-mode rjsx-mode))
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(straight-use-package-by-default t)
 '(tab-width 2)
 '(tide-native-json-parsing t)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 1)
 '(uniquify-strip-common-suffix nil)
 '(vc-annotate-background-mode nil)
 '(warning-suppress-types '((comp)))
 '(web-mode-enable-auto-quoting nil)
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
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 150 :width normal))))
 '(org-level-1 ((t (:height 2.0 :foreground "#DFAF8F" :extend nil :inherit default))))
 '(tide-hl-identifier-face ((t (:inherit region))))
 '(web-mode-interpolate-color1-face ((t (:inherit web-mode-css-property-name-face))))
 '(web-mode-interpolate-color2-face ((t (:inherit default)))))

(provide 'custom)
;;; custom.el ends here
