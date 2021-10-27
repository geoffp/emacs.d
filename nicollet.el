;;; Nicollet tools
;;; Commentary:
;;; This is a toolkit for working on the Nicollet team. :)
;;; Code:

;; Dired shortcuts

(defun nicollet-react ()
  (interactive)
  (find-file "~/src/nicollet/packages/nicollet-react"))

(defun web-nicollet-react ()
  (interactive)
  (find-file "~/src/web-nicollet/packages/react"))

(global-set-key (kbd "C-x w n r") 'web-nicollet-react)
(global-set-key (kbd "C-x n r") 'nicollet-react)



;;
;; STANDUP GENERATOR
;;
;; Current state:
;; - Creates a new buffer
;; - Dumps org-mode entry into it
;; - Iterates over lines
;;
;; TODO:
;; - Change stars into indented bullet list for Slack
;;
(defun create-standup ()
  "With the pointer on the day's entry of the work log, generate a standup post."
  (interactive)
  (setq standup-buffer (generate-new-buffer "Nicollet Standup"))
  (setq worklog-buffer (get-buffer "worklog.org"))
  (with-current-buffer worklog-buffer
    (setq entry (org-get-entry)))
  (with-current-buffer standup-buffer
    (insert entry)
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (forward-line))
    (switch-to-buffer standup-buffer)
    )
  )
