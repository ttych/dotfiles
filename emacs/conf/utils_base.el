;;;;;;;;;; editing
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;; (global-set-key (kbd "<C-return>") 'open-line-below)
;; (global-set-key (kbd "<C-S-return>") 'open-line-above)

;;;;;;;;;; hippe-expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))
(global-set-key (kbd "M-/") #'hippie-expand)

;;;;;;;;;; buffers
(require 'uniquify)
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style (quote reverse)
      uniquify-ignore-buffers-re "^\\*"
      uniquify-separator "/"
      )

;;;;;;;;;; dired
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)
;; dired - reuse current buffer by pressing 'a'
;; (put 'dired-find-alternate-file 'disabled nil)
;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)
;; (require 'dired-details)
;; (setq dired-details-initially-hide nil
      ;; setq-default dired-details-hidden-string "--- "
      ;; )
;; (dired-details-install)

;;;;;;;;;; abbrev <C-x a>
;; save abbrevs when files are saved
(setq-default abbrev-mode t)
(setq save-abbrevs 'silent)
(setq abbrev-file-name (expand-file-name "abbrevs" user-emacs-directory))
(global-set-key (kbd "C-x a TAB") 'expand-abbrev)
(global-set-key (kbd "C-x a a") 'add-mode-abbrev)
(global-set-key (kbd "C-x a e") 'edit-abbrevs)
(global-set-key (kbd "C-x a k") 'kill-all-abbrevs)
(global-set-key (kbd "C-x a l") 'list-abbrevs)
(global-set-key (kbd "C-x a s") 'write-abbrev-file)
;; (dolist (hook '(erc-mode-hook
;;                 emacs-lisp-mode-hook
;;                 text-mode-hook))
;; (add-hook hook #'abbrev-mode))
