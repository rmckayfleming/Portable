;; -*- lexical-binding: t -*-
;; My personalizations

;; Visual Customizations
(set-face-attribute 'default nil
                    :font "Berkeley Mono"
                    :height 130)

(set-face-attribute 'variable-pitch nil
                    :font "Berkeley Mono Variable"
                    :height 130)

;; Setup org-modern to make org-mode docs look a little nicer.
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :config
  (set-face-attribute 'org-modern-symbol nil :font "Hack Nerd Font" :height 130))

(setq org-pretty-entities t)

(load-theme 'doom-oksolar-light t)

(remacs-set-prism-colors)

(setq dimmer-fraction 0.15)

(require 'remacs-org)
(require 'remacs-emacs-lisp)
(require 'remacs-common-lisp)
(require 'remacs-clojure)
(require 'remacs-web)

(with-eval-after-load 'sly
  (setq inferior-lisp-program "/opt/homebrew/bin/sbcl"))
