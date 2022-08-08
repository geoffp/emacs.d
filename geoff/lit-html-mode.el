;;; lit-html-mode -- A major mode for editing lit-html templates.
;;
;; Author: Geoff Pursell
;; Maintainer: Geoff Pursell
;; Copyright (C) 2020-2022, Geoff Pursell
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))

;; Keywords: lit, lit-element, lit-html web components
;;; Commentary:
;;
;;; Code:

;;;###autoload
(define-derived-mode lit-html-mode html-mode "Lit-HTML Mode"
  "Major mode for editing Lit-HTML templates."

  (let ((st (lit-html-mode-syntax-table)))
         (modify-syntax-entry ?\" ".   " st)
         (modify-syntax-entry ?\\ ".   " st)))


  ;; (setq-local sgml-display-text html-display-text)
  ;; (setq-local sgml-tag-face-alist html-tag-face-alist)
  ;; (setq-local sgml-tag-alist html-tag-alist)
  ;; (setq-local sgml-face-tag-alist html-face-tag-alist)
  ;; (setq-local sgml-tag-help html-tag-help)
  ;; (setq-local outline-regexp "^.*<[Hh][1-6]\\>")
  ;; (setq-local outline-heading-end-regexp "</[Hh][1-6]>")
  ;; (setq-local outline-level
	;;   (lambda () (char-before (match-end 0))))
  ;; (setq-local add-log-current-defun-function #'html-current-defun-name)
  ;; (setq-local sentence-end-base "[.?!][]\"'”)}]*\\(<[^>]*>\\)*")

  ;; (when (fboundp 'libxml-parse-html-region)
  ;;   (defvar css-class-list-function)
  ;;   (setq-local css-class-list-function #'html-current-buffer-classes)
  ;;   (defvar css-id-list-function)
  ;;   (setq-local css-id-list-function #'html-current-buffer-ids))

  ;; (setq imenu-create-index-function 'html-imenu-index)

  ;; (setq-local sgml-empty-tags
	;;   ;; From HTML-4.01's loose.dtd, parsed with
  ;;   ;; `sgml-parse-dtd', plus manual additions of "source" and "wbr".
	;;   '("area" "base" "basefont" "br" "col" "frame" "hr" "img" "input"
  ;;      "isindex" "link" "meta" "source" "param" "wbr"))
  ;; (setq-local sgml-unclosed-tags
	;;   ;; From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd'.
	;;   '("body" "colgroup" "dd" "dt" "head" "html" "li" "option"
;; 	   "p" "tbody" "td" "tfoot" "th" "thead" "tr"))

(provide 'lit-html-mode)
;;; lit-html-mode.el ends here
