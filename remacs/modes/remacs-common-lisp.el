;; -*- lexical-binding: t -*-
;; remacs-common-lisp.el --- Configuration for Common Lisp modes

(require 'remacs-lisp)

;; Sly is essentially an IDE for Common Lisp. It's a descendant of SLIME.
(use-package sly)

(defun remacs-common-lisp-mode-defaults ()
  "Defaults for `common-lisp-mode'."
  (run-hooks 'remacs-lisp-hook) ; inherit all of the minor modes common to Lisps
  )

(setq remacs-common-lisp-mode-hook 'remacs-common-lisp-mode-defaults)

(add-hook 'lisp-mode-hook (lambda ()
                            (run-hooks 'remacs-common-lisp-mode-hook)))

(defun remacs-common-lisp-mrepl-mode-defaults ()
  "Defaults for interactive Lisp mode."
  (run-hooks 'remacs-interactive-lisp-hook))

(setq remacs-common-lisp-mrepl-mode-hook 'remacs-common-lisp-mrepl-mode-defaults)

(add-hook 'sly-mrepl-mode-hook (lambda ()
                                 (run-hooks 'remacs-common-lisp-mrepl-mode-hook)))

(with-eval-after-load 'sly-mrepl
  (define-key sly-mrepl-mode-map (kbd "s-<up>") #'sly-mrepl-previous-input-or-button)
  (define-key sly-mrepl-mode-map (kbd "s-<down>") #'sly-mrepl-next-input-or-button)
  (define-key sly-mrepl-mode-map (kbd "s-<return>") #'sly-mrepl-return)
  (define-key sly-mrepl-mode-map (kbd "<return>") nil)
  (define-key sly-mrepl-mode-map (kbd "RET") nil)
  (define-key sly-mrepl-mode-map (kbd ",") nil)
  (define-key sly-mrepl-mode-map (kbd "<backspace>") #'sp-backward-delete-char))

(provide 'remacs-common-lisp)
