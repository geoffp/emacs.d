;;; slack-post --- turn a work log week entry into a Slack post for the fellas

;;; Commentary:
;;; The process is:
;;; - grab the whole week entry
;;; - open a new buffer
;;; - alter the entry to suit an HTML export
;;; - paste the entry
;;; - export to html and open

;;; Code:

(require 'string-inflection)

(defun slack-post-string (S)
  "Slugify the string S."
  (string-inflection-camelcase-function (replace-regexp-in-string "[& ,+()*/?]+" "-" S)))

(defun slack-post (START END)
  "Slugify the region defined by START and END."
  (interactive "r")
  (let ((s (slack-post-string (delete-and-extract-region START END))))
    (message s)
    (insert s)))

;; (message (slack-post-string "you & I"))
;; (message (slack-post-string "This, is /ater (you?) & me +**"))

(provide 'slack-post)
;;; slack-post.el ends here
