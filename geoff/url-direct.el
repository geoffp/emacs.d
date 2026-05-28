;;; url-direct.el --- Bypass Zscaler for ELPA package archives  -*- lexical-binding: t; -*-

;; Author: You + Dayton
;; Description: Routes url.el requests for specified hosts through curl-direct,
;;   which binds to the physical network interface and bypasses the Zscaler tunnel.

;;; Commentary:
;;
;; Zscaler's SSL inspection proxy fails to relay TLS handshakes to
;; elpa.gnu.org and elpa.nongnu.org (and potentially other hosts).
;; This package intercepts `url-retrieve' and `url-retrieve-synchronously'
;; for those hosts and fulfills the request via `curl-direct' instead.
;;
;; Usage:
;;   (require 'url-direct)
;;   (url-direct-maybe-enable)  ; only activates if Zscaler is running
;;
;; Or force it on regardless:
;;   (url-direct-mode 1)
;;
;; Or with use-package:
;;   (use-package url-direct
;;     :load-path "~/.emacs.d/lisp"
;;     :config (url-direct-maybe-enable))

;;; Code:

(require 'url)
(require 'url-http)

(defgroup url-direct nil
  "Bypass network tunnel for specific hosts using curl."
  :group 'url
  :prefix "url-direct-")

(defcustom url-direct-hosts
  '("elpa.gnu.org" "elpa.nongnu.org")
  "List of hostnames that should be fetched via curl-direct."
  :type '(repeat string)
  :group 'url-direct)

(defun url-direct--zscaler-running-p ()
  "Return non-nil if Zscaler appears to be running on this machine."
  (cond
   ;; macOS: check for Zscaler processes
   ((eq system-type 'darwin)
    (zerop (call-process "pgrep" nil nil nil "-q" "-i" "zscaler")))
   ;; Linux: check for Zscaler processes (zscaler-service, etc.)
   ((eq system-type 'gnu/linux)
    (zerop (call-process "pgrep" nil nil nil "-i" "zscaler")))
   (t nil)))

(defcustom url-direct-curl-program
  (or (executable-find "curl-direct")
      (expand-file-name "~/bin/curl-direct"))
  "Path to the curl-direct script."
  :type 'string
  :group 'url-direct)

(defcustom url-direct-extra-curl-args '("-sS" "-L")
  "Extra arguments to pass to curl-direct.
-s = silent, -S = show errors, -L = follow redirects.
Note: Do NOT use --compressed here; it causes curl to decompress
the body while leaving Content-Encoding: gzip in the headers,
which confuses url-insert into attempting double decompression."
  :type '(repeat string)
  :group 'url-direct)

(defun url-direct--host-match-p (url-string)
  "Return non-nil if URL-STRING matches a host in `url-direct-hosts'."
  (when-let* ((url-obj (url-generic-parse-url url-string))
              (host (url-host url-obj)))
    (member (downcase host) url-direct-hosts)))

(defun url-direct--build-curl-args (url-string)
  "Build the argument list for curl-direct to fetch URL-STRING."
  (append url-direct-extra-curl-args
          ;; Include headers in output — package.el expects to find them
          ;; and searches for the blank line separator itself.
          (list "-i")
          (list url-string)))

(defun url-direct--setup-response-buffer ()
  "Set buffer-local variables that url.el/package.el consumers expect."
  (goto-char (point-min))
  ;; Strip \r from \r\n in HEADERS ONLY — mm-copy-to-buffer searches for
  ;; "^\n" to find the header/body separator and won't match if \r is
  ;; present.  We must not touch the body because binary content (e.g.
  ;; .tar files) may contain legitimate 0x0D 0x0A sequences.
  (let ((header-end (save-excursion
                      (if (re-search-forward "\r\n\r\n" nil t)
                          (point)
                        (point-max)))))
    (while (search-forward "\r\n" header-end t)
      (replace-match "\n" t t)))
  (goto-char (point-min))
  (when (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
    (setq-local url-http-response-status
                (string-to-number (match-string 1))))
  (when (re-search-forward "\n\n" nil t)
    (setq-local url-http-end-of-headers (point))))

(defun url-direct--retrieve-synchronously (url-string)
  "Fetch URL-STRING synchronously via curl-direct, return a buffer.
Does NOT change the caller's current buffer."
  (let ((buf (generate-new-buffer " *url-direct*"))
        (args (url-direct--build-curl-args url-string)))
    ;; Make buffer unibyte for binary-safe tar/gz content.
    ;; Use save-current-buffer so we never clobber the caller's context.
    (save-current-buffer
      (set-buffer buf)
      (set-buffer-multibyte nil))
    (let ((exit-code (apply #'call-process url-direct-curl-program
                            nil buf nil args)))
      (if (zerop exit-code)
          (progn
            (save-current-buffer
              (set-buffer buf)
              (url-direct--setup-response-buffer))
            buf)
        (message "url-direct: curl-direct failed (exit %d) for %s" exit-code url-string)
        (kill-buffer buf)
        nil))))

(defun url-direct--retrieve-async (url-string callback &optional cbargs)
  "Fetch URL-STRING via curl-direct and invoke CALLBACK.
Uses synchronous fetching (fast via curl-direct) and calls CALLBACK
immediately, since the network bypass makes this nearly instant.
CALLBACK is called with the response buffer as current buffer,
matching `url-retrieve' semantics."
  (let ((buf (url-direct--retrieve-synchronously url-string)))
    (if buf
        (save-current-buffer
          (set-buffer buf)
          (apply callback nil cbargs))
      (let ((err-buf (generate-new-buffer " *url-direct-error*")))
        (save-current-buffer
          (set-buffer err-buf)
          (apply callback `(:error (error ,(format "curl-direct failed for %s" url-string)))
                 cbargs))))
    buf))

(defun url-direct--advice-retrieve-synchronously (orig-fun url &rest args)
  "Advice for `url-retrieve-synchronously'.
If URL matches a direct host, use curl-direct; otherwise call ORIG-FUN."
  (let ((url-string (if (stringp url) url (url-recreate-url url))))
    (if (url-direct--host-match-p url-string)
        (or (url-direct--retrieve-synchronously url-string)
            (apply orig-fun url args))
      (apply orig-fun url args))))

(defun url-direct--advice-retrieve (orig-fun url callback &optional cbargs &rest args)
  "Advice for `url-retrieve'.
If URL matches a direct host, use curl-direct; otherwise call ORIG-FUN."
  (let ((url-string (if (stringp url) url (url-recreate-url url))))
    (if (url-direct--host-match-p url-string)
        (progn
          (message "url-direct: fetching %s via curl-direct" url-string)
          (url-direct--retrieve-async url-string callback cbargs))
      (apply orig-fun url callback cbargs args))))

;;;###autoload
(define-minor-mode url-direct-mode
  "Minor mode to route specific hosts through curl-direct, bypassing Zscaler."
  :global t
  :lighter " Direct"
  (if url-direct-mode
      (progn
        (advice-add 'url-retrieve-synchronously :around
                    #'url-direct--advice-retrieve-synchronously)
        (advice-add 'url-retrieve :around
                    #'url-direct--advice-retrieve)
        (message "url-direct-mode enabled: %s will bypass tunnel"
                 (string-join url-direct-hosts ", ")))
    (advice-remove 'url-retrieve-synchronously
                   #'url-direct--advice-retrieve-synchronously)
    (advice-remove 'url-retrieve
                   #'url-direct--advice-retrieve)
    (message "url-direct-mode disabled")))

;;;###autoload
(defun url-direct-maybe-enable ()
  "Enable `url-direct-mode' only if Zscaler is detected running.
Safe to call unconditionally on any platform."
  (if (url-direct--zscaler-running-p)
      (progn
        (url-direct-mode 1)
        (message "url-direct: Zscaler detected, mode enabled"))
    (message "url-direct: Zscaler not detected, mode not enabled")))

(provide 'url-direct)
;;; url-direct.el ends here
