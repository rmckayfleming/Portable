;; -*- lexical-binding: t -*-
;;; remacs-linux.el --- Linux specific config

;; When opening emacs as a GUI app, emacs gets a minimal set of environment
;; variables. This package by asking the shell to print out variables
;; and then copies them over.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'remacs-linux)
