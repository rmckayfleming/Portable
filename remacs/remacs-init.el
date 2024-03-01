;; -*- lexical-binding: t -*-

;; init.el --- Remacs's configuration entry point.

(defun remacs-message (format-string &rest args)
  "Wrapper around `message' to add [Remacs] to the front of the message."
  (apply 'message (concat "[Remacs] " format-string) args))

(remacs-message "Remacs is starting up...")

(defvar remacs-packages-dir (expand-file-name "packages" remacs-dir)
  "The directory containing remacs's core packages.")
(defvar remacs-modes-dir (expand-file-name "modes" remacs-dir)
  "The directory containing major mode specific customizations.")

;; General directory variables
(defvar savefile-dir (expand-file-name "savefile" scratch-dir)
  "This folder stores all of the auto-generated save/history files.")
(defvar dot-config-dir (expand-file-name ".config" (getenv "HOME"))
  "The user's .config directory.")

;; Add core/ and modules/ to the load-path (that is, they will be searched
;; when attempting to load a package).
(add-to-list 'load-path remacs-packages-dir)
(add-to-list 'load-path remacs-modes-dir)

(remacs-message "Loading Remacs's core modules...")

;; load the core stuff
(require 'remacs-packages)
(require 'remacs-ui)
(require 'remacs-core)
(require 'remacs-editor)
(require 'remacs-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'remacs-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'remacs-linux))

(remacs-message "Loading your personalization file...")

(load (expand-file-name "personalization.el" remacs-dir))

(setq custom-file (expand-file-name "custom.el" emacs-dir))
(load custom-file)

(remacs-message "Remacs is ready!")
