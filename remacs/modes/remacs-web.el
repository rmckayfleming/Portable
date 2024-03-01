;; -*- lexical-binding: t -*-
;; remacs-web.el --- Configuration for JSON modes.

(defun remacs-web-defaults ()
  "Default hook for various web modes (JSON, HTML, JS, CSS)."
  (aggressive-indent-mode))

;; Configure JSON and JS editing preferences.
(use-package json-mode)
(add-hook 'json-mode-hook 'remacs-web-defaults)

(setq-default js-indent-level 2)

;; Set up web-mode (instead of html-mode).
(use-package web-mode)
(add-hook 'web-mode-hook 'remacs-web-defaults)

;;; Set up associations for files.
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'remacs-web)
