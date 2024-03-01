;; -*- lexical-binding: t -*-
;;; remacs-core.el --- Core Remacs functions

(defun remacs-kill-buffer ()
  "Actually kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun remacs-keyboard-quit ()
  "Like keyboard-escape-quit, but doesn't close windows."
  (interactive)
  (cond
   ((eq last-command 'mode-exited) nil)
   ((region-active-p)
    (deactivate-mark))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (current-prefix-arg
    nil)
   ((> (recursion-depth) 0)'
    (exit-recursive-edit))
   (buffer-quit-function
    (funcall buffer-quit-function))))

(provide 'remacs-core)
