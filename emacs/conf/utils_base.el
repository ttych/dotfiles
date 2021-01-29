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

;;;;;;;;;; bookmark <M-_ b>
(setq bookmark-default-file (expand-file-name "bookmarks" save-dir)
      bookmark-save-flag 1)
(global-set-key (kbd "M-_ b s") 'bookmark-set)
(global-set-key (kbd "M-_ b j") 'bookmark-jump)
(global-set-key (kbd "M-_ b l") 'bookmark-bmenu-list)

;;;;;;;;;; recentf <M-_ e r> <f9>
(recentf-mode 1)
(setq recentf-save-file (expand-file-name "recentf" save-dir)
      recentf-max-menu-items 50
      recentf-max-saved-items 100
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never
      )
(global-set-key (kbd "M-_ e r") 'recentf-open-files)
(global-set-key (kbd "<f9>") 'recentf-open-files)
;; update list periodically, every 5 minutes
(run-at-time nil (* 5 60) 'recentf-save-list)

;;;;;;;;;; savehist
(savehist-mode 1)
(setq
 ;; search entries
 savehist-additional-variables '(search-ring regexp-search-ring)
 ;; save every 5 minutes
 savehist-autosave-interval (* 5 60)
 ;; keep the home clean
 savehist-file (expand-file-name "savehist" save-dir)
 )
