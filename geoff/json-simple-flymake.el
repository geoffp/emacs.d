;;;;;; 2023-08-02 mokrates
;;;;;; really simple but standalone json flymake utilizing the builtin json parser
;;;;;;
;;;;;; Modified for Emacs 30: json parse errors now signal
;;;;;; (SYMBOL LINE COLUMN POSITION) in condition-case, not
;;;;;; (MESSAGE LINE COLUMN) as in earlier versions. Original code
;;;;;; used wrong indices and passed an integer as the diagnostic
;;;;;; text, causing "wrong-type-argument listp N".

;;; customize: json-simple-flymake--add-to-js-mode-hook
;;; to start this automatically with javascript-mode

(defun json-simple-flymake (report-fn &rest _args)
  "Flymake backend for JSON using built-in json-parse-buffer.
In Emacs 30, json parse errors signal (SYMBOL LINE COLUMN POSITION)."
  (save-excursion
    (goto-char (point-min))
    (let ((result
           (condition-case err
               (progn (json-parse-buffer) nil)
             ((json-end-of-file json-error json-object-too-deep json-out-of-memory json-parse-error)
              (let ((fly-regn (flymake-diag-region (current-buffer) (nth 1 err) (nth 2 err)))
                    (msg (or (get (car err) 'error-message) "JSON parse error")))
                (list (flymake-make-diagnostic (current-buffer) (car fly-regn) (cdr fly-regn) :error msg)))))))
      (funcall report-fn (or result nil)))))

(defun json-simple-setup-flymake-backend ()
  (make-variable-buffer-local 'flymake-diagnostic-functions)  
  (add-hook 'flymake-diagnostic-functions 'json-simple-flymake)

  (when json-simple-flymake--enable-help
    (if (listp help-at-pt-display-when-idle)
	(add-to-list 'help-at-pt-display-when-idle 'flymake-diagnostic)
      (setf help-at-pt-display-when-idle '(flymake-diagnostic)))
    (help-at-pt-set-timer)))

(defgroup json-simple-flymake-group
  nil
  "json-simple-flymake: a simple flymake checker for json")

(defcustom json-simple-flymake--add-to-js-mode-hook nil
  "Add json-simple-flymake to js-mode-hook.
This is only really useful, if you don't edit javascript much but
only json. This is NOT a javascript linter"
  :type 'boolean
  :group 'json-simple-flymake-group)

(defcustom json-simple-flymake--enable-help t
  "enable help at point for json-simple-flymake"
  :type 'boolean
  :group 'json-simple-flymake-group)

(when json-simple-flymake--add-to-js-mode-hook
  (add-hook 'js-mode-hook 'json-simple-setup-flymake-backend)
  (add-hook 'js-mode-hook (lambda () (flymake-mode t))))
