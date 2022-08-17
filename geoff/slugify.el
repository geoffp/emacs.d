;;; slugify --- make a string into a camelCase id
;;; Commentary:

;;; Code:

(require 'string-inflection)

(defun slugify-string (S)
  "Slugify the string S."
  (string-inflection-camelcase-function (replace-regexp-in-string "[& ,+()*/?]+" "-" S)))

(defun slugify (START END)
  "Slugify the region defined by START and END."
  (interactive "r")
  (let ((s (slugify-string (delete-and-extract-region START END))))
    (message s)
    (insert s)))

;; (message (slugify-string "you & I"))
(message (slugify-string "This, is /ater (you?) & me +**"))

(provide 'slugify)
;;; slugify.el ends here
