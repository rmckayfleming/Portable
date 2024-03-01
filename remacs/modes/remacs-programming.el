;; -*- lexical-binding: t -*-
;;; remacs-programming --- Common programming configuration

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

(defun remacs-prog-mode-defaults ()
  "Default programming hook, useful with any programming language."
  (setq show-trailing-whitespace t)
  (smartparens-mode +1))

(setq remacs-prog-mode-hook 'remacs-prog-mode-defaults)
(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'remacs-prog-mode-hook)))

(provide 'remacs-programming)
