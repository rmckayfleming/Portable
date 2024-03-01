;; -*- lexical-binding: t -*-
;;; remacs-lisp.el --- Common config for Lisps

(require 'remacs-programming)

(defun remacs-lisp-defaults ()
  "Default hook for Lisp like languages."
  (prism-mode)
  (aggressive-indent-mode))

(setq remacs-lisp-hook 'remacs-lisp-defaults)

(defun remacs-interactive-lisp-defaults ()
  "Default hook for interactive Lisp sessions (think REPLs)"
  (prism-mode)
  (smartparens-mode +1))

(setq remacs-interactive-lisp-hook 'remacs-interactive-lisp-defaults)

(provide 'remacs-lisp)
