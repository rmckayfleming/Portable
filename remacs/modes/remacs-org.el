;; -*- lexical-binding: t -*-
;; remacs-org.el --- A bunch of customizations to org-mode.

(setq-default org-return-follows-link nil)

;; Electric indent mode is enabled by default for all modes, we want to turn it off
;; in org mode.
(defun disable-electric-indent-for-mode ()
  (electric-indent-local-mode -1))
(add-hook 'org-mode 'disable-electric-indent-for-mode)

(defun remacs/org-maybe-insert-item ()
  "Inserts a new list item with behaviour similar to Apple Notes.
That is, if the current item isn't empty it'll insert a new item, otherwise
it'll outdent or delete it."
  (when (org-in-item-p)
    (if (org-element-property :contents-begin (org-element-context))
        (org-insert-item)
      (condition-case nil
          (org-outdent-item)
        (error
         (beginning-of-line)
         (delete-region (line-beginning-position) (line-end-position)))))))

(defun remacs/org-return ()
  "Contextual newline. Does different things based on the current element."
  (interactive)
  (cond
   ((eq 'line-break (car (org-element-context)))
    (org-return-indent))

   ;; Open links like usual, unless point is at the end of a line.
   ;; and if at beginning of line, just press enter.
   ((or (and (eq 'link (car (org-element-context)))
             (not (eolp)))
        (bolp))
    (org-return))

   ;; Lists end with two blank lines, so we need to make sure we are also not
   ;; at the beginning of a line to avoid a loop where a new entry gets created
   ;; with only one blank line.
   ((org-in-item-p)
    (remacs/org-maybe-insert-item))

   ((org-at-table-p)
    (if (-any? (lambda (x) (not (string= " " x)))
               (nth (- (org-table-current-dline) 1)
                    (org-table-to-lisp)))
        (org-return)
      (beginning-of-line)
      (cl--set-buffer-substring (line-beginning-position) (line-end-position) "")
      (org-return)))

   (t (org-return))))

(with-eval-after-load 'org
  ;; Customize the keybindings

  ;; remacs/org-return instead of org-return
  (define-key org-mode-map (kbd "<return>") #'remacs/org-return)
  (define-key org-mode-map (kbd "RET") #'remacs/org-return)
  (define-key org-mode-map (kbd "S-<return>") #'org-return)
  (define-key org-mode-map (kbd "S-RET") #'org-return)

  ;; Remove default Shift-Left/Right behaviour.
  (define-key org-mode-map (kbd "S-<left>") nil)
  (define-key org-mode-map (kbd "S-<right>") nil)
  (define-key org-mode-map (kbd "S-M-<left>") #'org-shiftleft)
  (define-key org-mode-map (kbd "S-M-<right>") #'org-shiftright)

  ;; Make TAB act as an indent/outdent. M-TAB for cycling visibility.
  (define-key org-mode-map (kbd "TAB") #'org-indent-item)
  (define-key org-mode-map (kbd "<tab>") #'org-indent-item)
  (define-key org-mode-map (kbd "S-TAB") #'org-outdent-item)
  (define-key org-mode-map (kbd "S-<tab>") #'org-outdent-item)

  (define-key org-mode-map (kbd "M-TAB") #'org-cycle)
  (define-key org-mode-map (kbd "M-<tab>") #'org-cycle)
  (define-key org-mode-map (kbd "S-M-TAB") #'org-shifttab)
  (define-key org-mode-map (kbd "S-M-<tab>") #'org-shifttab))

(provide 'remacs-org)
