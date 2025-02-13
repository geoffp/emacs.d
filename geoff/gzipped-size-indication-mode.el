;;; gzipped-size-indication-mode --- A minor mode to display the size of the buffer when gzipped
;;; Commentary:
;;; It's my first try at a minor mode, let's go with it

;;; Code:

(defun get-gzip-size ()
  "Return the gzipped size of the current buffer in bytes."
  (save-excursion
    (let ((out-buffer-name "gzip command output"))
      (call-process-region 1 (buffer-size) "gzip" nil out-buffer-name nil "-c")
      (let* ((out-buffer (get-buffer out-buffer-name)) (size (buffer-size out-buffer)))
        (kill-buffer out-buffer)
        size))))

(defun buffer-text (&optional buffer-name)
  "Retrieve the plain text of the buffer BUFFER-NAME."
  (save-window-excursion
    (if buffer-name (switch-to-buffer buffer-name t))
    (buffer-substring-no-properties 1 (buffer-size))
    ))

(defun display-gzipped-size ()
  "Display the gzipped size of the current file in the minibuffer."
  (interactive)
  (let ((size (get-gzip-size)) (prefix "gzipped size: "))
    (message (if (< size 1000)
                 (format "%s%d bytes" prefix size)
               (format "%s%.2f KB" prefix (/ size 1000.0))))))

(defun update-gzipped-size ()
  "Update the minor mode lighter with the gzipped size of the current buffer."
  (if (gzipped-size-indication-mode-enabled)
      (let ((size (get-gzip-size)) (prefix " gz:"))
        (setcar (cdr (assq 'gzipped-size-indication-mode minor-mode-alist))
                (if (< size 1000) (format "%s%db" prefix size)
                  (format "%s%.2fk" prefix (/ size 1000.0))))
        ;; apparently the rendered minor mode name won't update until a key is
        ;; pressed! unless we do this
        (force-window-update)
        (redisplay))))

(defun gzipped-size-indication-mode-enabled ()
  "Return non-nil if the current buffer has \"gzipped-size-indication-mode\" enabled."
  (member 'gzipped-size-indication-mode local-minor-modes))

(define-minor-mode gzipped-size-indication-mode
  "A minor mode to display the gzipped size of the current buffer."
  :lighter " gz"
  :after-hook (let ((enabling (gzipped-size-indication-mode-enabled)))
                (cancel-function-timers 'update-gzipped-size)
                (if enabling (progn
                               (update-gzipped-size)
                               (run-with-idle-timer 3 t 'update-gzipped-size)
                               (add-hook 'after-save-hook 'update-gzipped-size nil t))
                  ;; disabling
                  (remove-hook 'after-save-hook 'update-gzipped-size t))))


(provide 'gzipped-size-indication-mode)
;;; gzipped-size-indication-mode.el ends here
