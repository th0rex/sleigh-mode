;;; sleigh-mode.el -*- lexical-binding: t; -*-

(defvar sleigh-mode-hook nil)

(defvar sleigh-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?#  "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defconst sleigh-font-lock-keywords
  (list
   '("define\\|register\\|include\\|is\\|attach\\|token" . font-lock-keyword-face)
   '("0x[0-9a-fA-F]*" . font-lock-constant-face)
   '("[-+]?\\b[0-9]*" . font-lock-constant-face)))

(defun sleigh-mode ()
  "Major mode for editing Ghidra Sleigh files (.slaspec, .sinc)."
  (interactive)
  (set (make-local-variable 'font-lock-defaults) '(sleigh-font-lock-keywords))
  (set-syntax-table sleigh-mode-syntax-table)
  (setq major-mode 'sleigh-mode)
  (setq mode-name "Sleigh")
  (run-hooks 'sleigh-mode-hook)
  (font-lock-ensure)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slaspec\\'" . sleigh-mode))

(provide 'sleigh-mode)
;;; sleigh-mode.el ends here
