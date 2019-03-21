;;; sleigh-mode.el -*- lexical-binding: t; -*-

(defvar sleigh-mode-hook nil)

(defvar sleigh-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?#  "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st))

(defconst sleigh-font-lock-keywords
  (list
   '("@[a-zA-Z0-9]+" . font-lock-preprocessor-face)
   '("\\_<\\(define\\|include\\|is\\|attach\\|token\\|unimpl\\|call\\|goto\\|if\\|return\\|build\\|export\\|macro\\)\\_>" . font-lock-keyword-face)
   '("\\_<\\(sext\\|zext\\|carry\\|scarry\\|sborrow\\|nan\\|abs\\|sqrt\\|int2float\\|float2float\\|trunc\\|ceil\\|floor\\|round\\|cpool\\|newobject\\|delayslot\\)\\_>" . font-lock-builtin-face)
   '(":[a-zA-Z][a-zA-Z0-9_.]*" . font-lock-type-face)
   '("\\_<[a-zA-Z][a-zA-Z0-9_]*(" . font-lock-function-name-face)
   '("\\_<[a-zA-Z][a-zA-Z_0-9]*=" . font-lock-variable-name-face)
   '("\\_<& \\([a-zA-Z][a-zA-Z_0-9]*\\)" . (1 font-lock-variable-name-face))
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
  ;; Ghetto way of enabling todo highlighting, do this properly.
  (run-hooks 'prog-mode-hook)
  (font-lock-ensure)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slaspec\\'\\|\\.sinc\\'" . sleigh-mode))

(provide 'sleigh-mode)
;;; sleigh-mode.el ends here
