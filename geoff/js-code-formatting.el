;;; js-code-formatting --- select and enable biome or prettier code formatting based on configs found in the project
;;; Commentary:

;; TODO
;; - [ ] Pick a better data type for the defcustom
;; - [x] Make it actually work
;; - [ ] Detect installed prettier and biome modes before attempting to use them

;;; Code:

(require 'cl-extra)

(defcustom js-code-formatting--modes
  '(typescript-mode
     typescript-ts-mode
     tsx-ts-mode
     js-mode
     js-ts-mode
     web-mode
     rjsx-mode
     js2-mode
     javascript-mode)
  "The modes in which to automatically enable JS code formatting."
  :group 'js-code-formatting
  :type 'sexp)

(defcustom js-code-formatting--biome-config-filenames
  '("biome.json"  "biome.jsonc")
  "The known names of Biome config files, searched for at project roots to determine whether to use Biome as the code formatter."
  :group 'js-code-formatting
  :type 'sexp)

(defcustom js-code-formatting--prettier-config-filenames
  '(".prettierrc"
     ".prettierrc.js"
     ".prettierrc.json"
     ".prettierrc.json5"
     ".prettierrc.ts"
     ".prettierrc.yaml"
     ".prettierrc.yml"
     "prettier.config.js"
     "prettier.config.ts"
     ".prettierrc.mjs"
     "prettier.config.mjs"
     ".prettierrc.mts"
     "prettier.config.mts"
     ".prettierrc.cjs"
     "prettier.config.cjs"
     ".prettierrc.cts"
     "prettier.config.cts"
     ".prettierrc.toml")
  "The known names of Prettier config files, searched for at project roots to determine whether to use Prettier as the code formatter."
  :group 'js-code-formatting)

;; Code formatting enablement; prettier or biome?
(defun js-code-formatting-buffer-enable ()
  "Enable biome or prettier in current buffer."
  (interactive)
  (let ((project-root-dir (project-root (project-current)))
         (biome-found nil)
         (prettier-found nil))
    (cond
      ((cl-some
         #'(lambda (f) (file-exists-p (concat project-root-dir f)))
         js-code-formatting--biome-config-filenames)
        (progn
          (message "Biome config found; enabling biome code formatting.")
          (biomejs-format-mode)))
      ((cl-some
         #'(lambda (f) (file-exists-p (concat project-root-dir f)))
         js-code-formatting--prettier-config-filenames)
        (progn
          (message "Prettier config found; enabling prettier code formatting.")
          (prettier-mode))))))

(defun js-code-formatting--hook-for-mode (mode)
  "Given a symbol MODE representing a JS-ish mode, return the symbol of its hook or nil if it doesn't exist."
  (let ((mode-name (symbol-name mode)))
    (intern-soft (concat mode-name "-hook"))))

(defun js-code-formatting--enable ()
  "Load up hooks for various JS-ish modes to automatically enable code formatting."
  (dolist (mode js-code-formatting--modes)
    ;; derive the hook symbol from the name of the mode
    (let ((hook (js-code-formatting--hook-for-mode mode)))
      (if hook (add-hook hook 'js-code-formatting-buffer-enable)))))

(defun js-code-formatting--disable ()
  "Load up hooks for various JS-ish modes to automatically enable code formatting."
  (dolist (mode js-code-formatting--modes)
    ;; derive the hook symbol from the name of the mode
    (let ((hook (js-code-formatting--hook-for-mode mode)))
      (if hook (remove-hook hook 'js-code-formatting-buffer-enable)))))

(js-code-formatting--enable)

;;; js-code-formatting.el ends here
