;; -*- lexical-binding: t -*-
;;; remacs-emacs-lisp.el --- Common configuration for Emacs lisp modes

(require 'remacs-lisp)

;; Bring in some keybindings similar to SLY
(define-key emacs-lisp-mode-map (kbd "C-C C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

;; Adds M-. and M-, for navigating to symbol definitions (a la SLY)
(use-package elisp-slime-nav
  :hook (emacs-lisp-mode ielm-mode))

;; Highlight function calls by underlining them
(use-package highlight-function-calls
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode) . highlight-function-calls-mode))

;; Emacs Lisp Mode defaults
(defun remacs-emacs-lisp-mode-defaults ()
  "Defaults for `emacs-lisp-mode'."
  (run-hooks 'remacs-lisp-hook)
  (eldoc-mode +1))

(setq remacs-emacs-lisp-mode-hook 'remacs-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'remacs-emacs-lisp-mode-hook)))

;; IELM (Interactive Emacs Lisp Mode) defaults
(defun ielm-init-history ()
  "Sets up history ring for IELM."
  (let ((path (expand-file-name "ielm/history" user-emacs-directory)))
    (make-directory (file-name-directory path) t)
    (setq-local comint-input-ring-file-name path))
  (setq-local comint-input-ring-size 10000)
  (setq-local comint-input-ignoredups t)
  (comint-read-input-ring))

(defun ielm-write-history (&rest _args)
  (with-file-modes #o600
    (comint-write-input-ring)))
(advice-add 'ielm-send-input :after 'ielm-write-history)

(defun remacs-ielm-mode-defaults ()
  "Defaults for `ielm'."
  (run-hooks 'remacs-interactive-lisp-hook)
  (eldoc-mode +1)
  (ielm-init-history))

(setq remacs-ielm-mode-hook 'remacs-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'remacs-ielm-mode-hook)))

(with-eval-after-load 'ielm
  (define-key ielm-map (kbd "RET") 'newline)
  (define-key ielm-map (kbd "s-<return>") 'ielm-return)
  (define-key ielm-map (kbd "s-<up>") 'comint-previous-input)
  (define-key ielm-map (kbd "s-<down>") 'comint-next-input))

;; Enable smartparens in the minibuffer
(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression.'"
  (if (eq this-command 'eval-expression)
      (smartparens-mode +1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'remacs-emacs-lisp)
