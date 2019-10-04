;;; sleigh.el --- Sleigh mode -*- lexical-binding: t; -*-
;;; Commentary:
;; Provides a major mode for Ghidra sleigh files.

;;; Code:

(defconst sleigh-re-ident "[[:word:]_][[:word:]_[:digit:].]*")

(defun sleigh-re-grab (x)
  "Build a regex that captures X."
  (concat "\\(" x "\\)"))

(defconst sleigh-re-grab-ident (sleigh-re-grab sleigh-re-ident))

(defconst sleigh-re-pp-def (concat "^@define " sleigh-re-grab-ident))
(defconst sleigh-re-addr-space (concat "^define space " sleigh-re-grab-ident))
(defconst sleigh-re-token (concat "^define token " sleigh-re-grab-ident))
(defconst sleigh-re-pcodeop (concat "^define pcodeop " sleigh-re-grab-ident))
(defconst sleigh-re-macro (concat "^macro " sleigh-re-grab-ident))
(defconst sleigh-re-constructor (concat "^" sleigh-re-grab-ident ":"))
(defconst sleigh-re-instr (concat "^:" sleigh-re-grab-ident)) ;; TODO: maybe include constructors

(defconst sleigh-builtins
  '(
    "abs"
    "carry" "ceil" "cpool"
    "delayslot"
    "float2float" "floor"
    "int2float"
    "nan" "newobject"
    "round"
    "sborrow" "scarry" "sext" "sqrt"
    "trunc"
    "zext"
    )
  )

(defconst sleigh-keywords
  '("attach"
    "build"
    "call"
    "define"
    "export"
    "goto"
    "if" "include" "is"
    "local"
    "macro"
    "return"
    "signed"
    "token"
    "unimpl")
  )

(defvar sleigh-font-lock-keywords
  `(
    ;; Keywords
    (,(concat "\\_<" (regexp-opt sleigh-keywords) "\\_>") . font-lock-keyword-face)

    ;; Pre-Processor directives
    ("@[[:word:]]*" . font-lock-preprocessor-face)
    ;; Pre-Processor keywords
    ("defined" . font-lock-preprocessor-face)

    ;; Constructors
    (,(concat (sleigh-re-grab sleigh-re-ident) ":[^:]") 1 font-lock-variable-name-face)
    ;; Instructions
    (,sleigh-re-instr 1 font-lock-variable-name-face)

    ;; Builtins
    (,(concat (sleigh-re-grab (regexp-opt sleigh-builtins)) "(") 1 font-lock-builtin-face)
    )
  )

(defvar sleigh-imenu-generic-expression
  `(;; ("Register" sleigh-re-reg 1) ;; TODO: multiple per line possible, what do we do?
    ("Pre-Processor Define" ,sleigh-re-pp-def 1)
    ("Address Space" ,sleigh-re-addr-space 1)
    ("Token" ,sleigh-re-token 1)
    ("PCodeOp" ,sleigh-re-pcodeop 1)
    ("Macro" ,sleigh-re-macro 1)
    ("Constructor" ,sleigh-re-constructor 1)
    ("Instruction" ,sleigh-re-instr 1)
    )
  )

(defvar sleigh-syntax-table
  (let ((table (make-syntax-table)))
    ;; TODO: Is this hacky? Treat = as an operator so we get highlighting of numbers (i.e. 0 in "XXX=0").
    (modify-syntax-entry ?= "." table)

    ;; Words include _ and .
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?_ "_" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)

    ;; Comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\^m ">" table)
    table))

;;;###autoload
(define-derived-mode sleigh-mode prog-mode "Sleigh"
  "Major mode for Ghidra Sleigh files."
  :syntax-table sleigh-syntax-table

  (setq-local font-lock-defaults '(sleigh-font-lock-keywords))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local open-paren-in-column-0-is-defun-start nil)

  (setq-local imenu-generic-expression sleigh-imenu-generic-expression)

  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slaspec\\'\\|\\.sinc\\'" . sleigh-mode))

(provide 'sleigh)
;;; sleigh.el ends here
