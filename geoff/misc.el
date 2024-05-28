;;; misc --- random stuff I've written that doesn't belong elsewhere
;;; Commentary:

;;; Code:

(defun edit-init ()
  "Edit the init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Work in progress. Taken from:
;; https://github.com/Qquanwei/emacs/blob/master/lisp/css-rem-convert.el
(defun css-rem-convert ()
  "Convert between CSS pixels and rems."
  (interactive)
  (setq debug-on-error t)
  (message "hi")
  (let ((text (sexp-at-point))
        (location (bounds-of-thing-at-point 'sexp)))
    (message "converting: %s at %s" text location)
    (cond
     ((s-ends-with? "rem" text)
      (delete-region (car location) (cdr location))
      (message "using font-size: %d" 16)
      (insert (format "%dpx" (* 16 (string-to-number text)))))
     ((s-ends-with? "px" text)
      (delete-region (car location) (cdr location))
      (insert (format "%.2frem" (/ (string-to-number text) 16)))))
    )
  )

(defun toggle-debug ()
  "Toggle CSS SVG debugging rule on and off. Written for an SVG animation job."
  (interactive)
  (save-excursion
    (goto-char 0)
    (if (not (eq (search-forward "@layer debug, main;" nil t) nil))
        (progn
          (replace-match "@layer main, debug;")
          (message "Debug mode active."))
      (unless (eq (search-forward "@layer main, debug;" nil t) nil)
        (progn
          (replace-match "@layer debug, main;")
          (message "Debug mode disabled.")
          )))))

(provide 'misc)
;;; misc.el ends here
