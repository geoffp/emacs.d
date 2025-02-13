;;; gzipped-size-indication-mode --- A minor mode to display the size of the buffer when gzipped
;;; Commentary:
;;; It's my first try at a minor mode, let's go with it

;;; Code:

(defun get-gzip-size ()
  "Return the gzipped size of the file FILENAME."
  (shell-command
   (format "cat << EOM | gzip -c | wc -c\n%s\nEOM\n"
           (buffer-text))
   "gzip-result")
  (string-to-number (buffer-text "gzip-result")))

(defun buffer-text (&optional buffer-name)
  "Retrieve the plain text of the buffer BUFFER-NAME."
  (save-window-excursion
    (if buffer-name (switch-to-buffer buffer-name t))
    (buffer-substring-no-properties 1 (buffer-size))
    ))

(defun display-gzip-size ()
  "Display the gzipped size of the current file in the minibuffer."
  (interactive)
  (let ((file (buffer-file-name)) (size (get-gzip-size)) (prefix "gzipped size: "))
    (message (if (< size 1000)
                 (format "%s%d bytes" prefix size)
               (format "%s%.2f KB" prefix (/ size 1000.0))))))

(defun update-gzip-size ()
  "Update the minor mode lighter with the gzipped size of the current buffer."
  (if (gzipped-size-indication-mode-enabled)
      (let ((file (buffer-file-name)) (size (get-gzip-size)) (prefix " gz:"))
        (setcar (cdr (assq 'gzipped-size-indication-mode minor-mode-alist))
                (if (< size 1000)
                    (format "%s%db" prefix size)
                  (format "%s%.2fk" prefix (/ size 1000.0)))))))

(defun gzipped-size-indication-mode-enabled ()
  "Return non-nil if the current buffer has \"gzipped-size-indication-mode\" enabled."
  (member 'gzipped-size-indication-mode local-minor-modes))

(define-minor-mode gzipped-size-indication-mode
  "A minor mode to display the gzipped size of the current buffer."
  :lighter " gz"
  :after-hook (let ((enabling (gzipped-size-indication-mode-enabled)))
                (cancel-function-timers 'update-gzip-size)
                (if enabling (progn
                               (update-gzip-size)
                               (run-with-idle-timer 3 t 'update-gzip-size)
                               (add-hook 'after-save-hook 'update-gzip-size nil t))
                  ;; disabling
                  (remove-hook 'after-save-hook 'update-gzip-size t))))


(provide 'gzipped-size-indication-mode)
;;; gzipped-size-indication-mode.el ends here
