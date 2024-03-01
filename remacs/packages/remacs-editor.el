;; -*- lexical-binding: t -*-
;;; remacs-editor.el --- enhanced core editing experience

(setq-default indent-tabs-mode nil) ; don't use tabs for indentation
(setq-default tab-width 4) ;; but when they do appear, maintain a good appearance

;; Default to UTF-8 encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)

;; Ensure new lines at the end of files
(setq require-final-newline t)

;; Enable CUA mode, but don't enable its default bindings (we'll set our own later).
;; CUA mode essentially introduces shift selection, selection highlighting, and deletion of the selection (which is what most new users expect)
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; Enable paren matching
(require 'paren)
(setq show-paren-when-point-inside-paren t
      show-paren-style 'parenthesis)
(show-paren-mode +1)
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; use the bar cursor by default
(setq-default cursor-type 'bar)

;; revert buffers automatically when the underlying files are changed by some other process
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t) ; this also enables auto-updating things like Dired which aren't backed by files

;; Aggressive indent automatically indents after every change. No need to think about it.
(use-package aggressive-indent)

;; Smartparens is a fantastic package for structural editing (super useful with Lisps)
(use-package smartparens
  :bind
  (:map smartparens-mode-map
        (("C-<backspace>" . #'sp-backward-kill-sexp)
         ("C-<right>" . #'sp-forward-slurp-sexp)
         ("C-<left>" . #'sp-forward-barf-sexp)
         ("C-M-<left>" . #'sp-backward-slurp-sexp)
         ("C-M-<right>" . #'sp-backward-barf-sexp)
         ("M-<left>" . #'sp-backward-symbol)
         ("M-<right>" . #'sp-forward-symbol)))
  :config
  (require 'smartparens-config))

;; Prism is useful for Lisps. It highlights expressions based on their depth.
;; Makes it really easy to grok S-expresions and tell if they're balanced or not.
(use-package prism
  :quelpa (prism :fetcher github :repo "alphapapa/prism.el"))

(defun remacs-set-prism-colors ()
  "Sets Prisms colors based on the currently enabled doom theme."
  (prism-set-colors :lightens '(0 -5 -10) :desaturations '(-2.5 0 2.5)
    :colors (-map #'doom-color '(red orange yellow green blue violet))
    :strings-fn (lambda (color)
                  (--> color
                       (color-lighten-name it -20)))))

(provide 'remacs-editor)
