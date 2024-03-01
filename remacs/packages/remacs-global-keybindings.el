;; -*- lexical-binding: t -*-
;;; remacs-global-keybindings.el --- Global keybindings

;;; Now set a bunch of common keybindings to make emacs comfier for new users. Most of these go on Super (Cmd)

(global-set-key (kbd "s-x") 'kill-region) ; cut
(global-set-key (kbd "s-c") 'copy-region-as-kill) ; copy
(global-set-key (kbd "s-v") 'yank) ; paste

(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-Z") 'undo-redo)

(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-o") 'counsel-find-file)
(global-set-key (kbd "s-r") 'ivy-switch-buffer)
(global-set-key (kbd "s-w") 'remacs-kill-buffer) ; kill the current buffer
(global-set-key (kbd "s-W") 'quit-window) ; kill the current window and bury its buffer

(global-set-key (kbd "s-l") 'display-line-numbers-mode) ; Toggle line numbers in gutter

(defun prompt-kill-emacs ()
  "Asks the user if they want to kill emacs, and then does it."
  (interactive)
  (if (y-or-n-p "Quit emacs?")
      (save-buffers-kill-terminal)))
(global-set-key (kbd "s-q") 'prompt-kill-emacs)


(keymap-global-set "<escape>" 'remacs-keyboard-quit) ; make ESC be C-g (for the most part that is)

;; Emulate the Cocoa text system shortcuts (Meta left/right already skips words)
(global-set-key (kbd "s-<left>") 'beginning-of-visual-line)
(global-set-key (kbd "s-<right>") 'end-of-visual-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; Comint mode is the basis for many interactive prompts (like REPLs and CLIs)
;; Set the basic keys
(with-eval-after-load 'comint-mode
  (define-key comint-mode-map (kbd "s-<up>") #'comint-previous-input)
  (define-key comint-mode-map (kbd "s-<down>") #'comint-next-input))

(provide 'remacs-global-keybindings)
