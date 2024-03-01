;; -*- lexical-binding: t -*-
;;; remacs-macos.el --- macOS Specific config

;; Adjust the modifier key meanings
(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-control-modifier 'control
      mac-function-modifier 'hyper
      mac-right-option-modifier 'meta)
;; As an aside, you might be interested in this: https://notes.alexkehayias.com/emacs-natural-title-bar-with-no-text-in-macos/
(setq mac-use-title-bar nil
      ns-use-native-fullscreen t)

;; No point in hiding the menu bar on macOS, so let's not do it 
(menu-bar-mode 1)

;; When opening emacs as a GUI app, emacs gets a minimal set of environment
;; variables. This package by asking the shell to print out variables
;; and then copies them over.
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'remacs-macos)
