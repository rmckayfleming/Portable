;; -*- lexical-binding: t -*-
;;; remacs-ui.el --- Some basic UI tweaks

;; Disable the toolbar
(tool-bar-mode -1)

;; Disable vertical scroll bar
(scroll-bar-mode -1)

;; Disable tooltips
(tooltip-mode -1)

;; Disable the splash screen
(setq inhibit-startup-screen t)

;; enable y/n answers instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(set-fringe-mode 10) ; gives some breathing room around the sides of windows

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Load some packages that are mostly UI/theme related
(use-package nerd-icons) ; Used by doom-modeline

(use-package doom-modeline
  :init (doom-modeline-mode 1)) ; Nice default mode line

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-mode t)) ; Visually dims non-active windows.

(use-package doom-themes
  :config
  ;; If either of the following are set to nil, then bold/italic are universally disabled (respectively)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config) ; Flash the mode line on errors rather than using the bell.
  (doom-themes-org-config)) ; Corrects (and improves) org-mode's native fontification

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Recommended by Mastering Emacs (https://www.masteringemacs.org/article/demystifying-emacs-window-manager)
;; Makes the buffer switching commands respect display actions
(setq switch-to-buffer-obey-display-actions t)

;; From Nano.el
;; Properties of the default frame
(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height . 45)
               '(width . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe . 1)
               '(right-fringe . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

;; Completion style in the minibuffer (gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html)
(setq completion-styles '(basic substring))

;; Minimum height for windows
(setq window-min-height 1)

;; Size of temporary buffers (such as *Help* buffers)
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Set up various history settings
(setq savehist-additional-variables
      '(kill-ring
        command-history
	    set-variable-value-history
	    custom-variable-history
	    query-replace-history
	    read-expression-history
	    minibuffer-history
	    read-char-history
	    face-name-history
	    bookmark-history
        ivy-history
	    counsel-M-x-history
	    file-name-history
        counsel-minibuffer-history))
(setq history-length 250)
(setq kill-ring-max 25)
(put 'minibuffer-history         'history-length 50)
(put 'file-name-history          'history-length 50)
(put 'set-variable-value-history 'history-length 25)
(put 'custom-variable-history    'history-length 25)
(put 'query-replace-history      'history-length 25)
(put 'read-expression-history    'history-length 25)
(put 'read-char-history          'history-length 25)
(put 'face-name-history          'history-length 25)
(put 'bookmark-history           'history-length 25)
(put 'ivy-history                'history-length 25)
(put 'counsel-M-x-history        'history-length 25)
(put 'counsel-minibuffer-history 'history-length 25)
(setq savehist-file (expand-file-name ".remacs-savehist" scratch-dir))
(savehist-mode 1)

;; Remove text properties for kill ring entries
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(setq bookmark-default-file (expand-file-name ".remacs-bookmark" scratch-dir))

;; Backup files
(setq backup-directory-alist (list (cons "." (expand-file-name ".remacs-backups" scratch-dir)))
      make-backup-files t     ; backup a file the first time it is saved.
      backup-by-copying t     ; don't clobber symlinks
      version-control t       ; version numbers for backup files
      delete-old-versions t   ; delete excess backup files silently
      kept-old-versions 6     ; oldest versions to keep when a new numbered backup
                                        ; is made (default: 2)
      kept-new-versions 9     ; newest versions to keep when a new numbered backup
                                        ; is made (default: 2)
      auto-save-default t     ; auto-save every buffer that visits a file
      auto-save-timeout 20    ; number of seconds idle time before auto-save
                                        ; (default: 30)
      auto-save-interval 200) ; number of keystrokes between auto-saves
                              ; (default: 300)

(provide 'remacs-ui)
