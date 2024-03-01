;; -*- lexical-binding: t -*-
;; remacs-common-lisp.el --- Configuration for Clojure modes

(require 'remacs-lisp)

(use-package clojure-mode)

(defun remacs-clojure-mode-defaults ()
  "Defaults for `clojure-mode'."
  (run-hooks 'remacs-lisp-hook) ; inherit all of the minor modes common to Lisps
  )

(setq remacs-clojure-mode-hook 'remacs-clojure-mode-defaults)

(provide 'remacs-clojure)
