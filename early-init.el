;;; early-init.el --- Early init  -*- lexical-binding: t; -*-

;;; Commentary:
;; Runs before init.el and before package-initialize.

;;; Code:

;; Allow MELPA packages to override built-in packages (e.g., transient).
;; Without this, Emacs 30's built-in transient 0.7.2 conflicts with the
;; newer MELPA version that magit requires.
(setq package-install-upgrade-built-in t)

;;; early-init.el ends here
