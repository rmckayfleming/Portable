;; -*- lexical-binding: t -*-
;;; remacs-packages.el --- Sets up package management, and a bunch of packages that apply across modes.

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
		("org" . "https://orgmode.org/elpa")
		("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;;; Update package definitions
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ; use :ensure nil to disable this where necessary

;;; Setup QUELPA (allowing us to use packages from git repos)
(use-package quelpa)
(when (package-installed-p 'quelpa)
  (quelpa '(quelpa-use-package
            :fetcher git
            :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

(use-package recentf
  :init
  ;; Enable recentf mode (which makes recently opened, but since closed, files show up in the switch buffer minibuffer)
  (setq recentf-max-menu-items 25)
  (setq recentf-save-file (expand-file-name ".remacs-recentf" scratch-dir))
  (recentf-mode 1))

;; We install org-mode early since we use it a lot. May as well make sure it loads properly before loading the rest of the config.
(use-package org
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-startup-indented t)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode))

;; Rest of the main packages...

;; Install command-log-mode, which when enabled, keeps track of what your keypresses do. This is useful for when you're using a new package and accidentally invoke a command and want to know what it is you did.
(use-package command-log-mode)

;; which-key is super helpful. When you press a prefix key (such as C-x), it'll show a popup of the available commands in that prefix key. This can be good for exploring.
(use-package which-key
  :config
  (setq which-key-sort-order 'which-key-description-order) ; sorts by the command name rather than keystroke
  (which-key-mode))

;; Ivy is a popular minibuffer completion mechanism.
(use-package ivy
  :config
  (setq ivy-initial-inputs-alist nil) ; By default ivy adds a "^" to the beginning of the completion (which is a regexp)
  (setq ivy-use-virtual-buffers t) ; Makes recently closed files still show up in the switch-buffer minibuffer
  (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line) ; Fixes highlighting the whole line
  (ivy-mode 1)
  :bind
  (("C-x b" . ivy-switch-buffer)))

;; Counsel is a collection of Ivy-enhanced versions of common Emacs commands (such as opening files,
;; describing functions, etc.)
(use-package counsel
  :config (counsel-mode)
  :bind
  (("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history)))

;; By default, Ivy is a bit bare in what it shows. Ivy-rich adds more information to the completions, such as
;; showing the key binding for a command, or the command's docstring text (for M-x that is).
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

;; Helpful makes the Help buffers nicer by providing more contextual information
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Where Ivy handles minibuffer completions, Company provides in-buffer completions (like autocomplete). We configure it to use TAB to complete, ESC to cancel completions, and RET is ignored.
(use-package company
  :bind
  (:map company-active-map
        ("<escape>" . #'company-abort)
        ("<return>" . nil)
        ("<tab>" . #'company-complete-selection)
        ("RET" . nil)
        ("TAB" . #'company-complete-selection))
  :config
  (global-company-mode))

;; Install magit for a nice git interface (some say the best).
(use-package magit)

(provide 'remacs-packages)
