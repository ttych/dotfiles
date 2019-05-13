;;; my-init.el --- Thomas Emacs configuration

;;; Commentary:

;;; Code:


(defconst *is-unix* (member system-type '(freebsd)))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (member system-type '(gnu gnu/linux gnu/kfreebsd)))


;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,savefile-dir)))
;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,savefile-dir t)))
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)




;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; hippie expand is dabbrev expand on steroids
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

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; align code in a pretty way
;(global-set-key (kbd "C-x \\") #'align-regexp)

;(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
;(global-set-key (kbd "s-<") #'beginning-of-buffer)
;(global-set-key (kbd "s->") #'end-of-buffer)
;(global-set-key (kbd "s-q") #'fill-paragraph)
;(global-set-key (kbd "s-x") #'execute-extended-command)

;; smart tab behavior - indent or complete
;(setq tab-always-indent 'complete)


;;; Packages :: additional

(use-package emamux
  :ensure t)


(use-package move-text
  :ensure t
  :bind  (([(control shift up)] . move-text-up)
		  ([(control shift down)] . move-text-down)))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode 1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (global-set-key (kbd "C-c C-r") 'recentf-open-files)
  (recentf-mode 1))

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
      bookmark-save-flag 1)

(use-package rainbow-mode
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode)
  )

(use-package webjump
  :ensure t
  :config
  (eval-after-load "webjump"
    '(add-to-list 'webjump-sites
                  '("Urban Dictionary" .
                    [simple-query
                     "www.urbandictionary.com"
                     "http://www.urbandictionary.com/define.php?term="
                     ""])))
  (global-set-key (kbd "C-c W") 'webjump)
  )

;; Add Urban Dictionary to webjump

;;; Packages :: theme
;; (use-package zenburn-theme
;;   :ensure t
;;   :config
;;   (load-theme 'zenburn t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties t))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; (use-package mandm-theme
;;   :ensure t
;;   :config
;;   (load-theme 'mandm t))


;;; Packages :: to_validate


(use-package abbrev
  :delight
  :config
  (setq save-abbrevs 'silent)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq-default abbrev-mode t)
  ;; :delight
  ;; (delight 'abbrev-mode " Abv" 'abbrev)
  )
;; C-x a g / C-u <x> C-x a g




;; emacs buffer name unique
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (use-package windmove
;;   :config
;;   ;; use shift + arrow keys to switch between visible buffers
;; (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package lisp-mode
  :config
  (defun user-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'user-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  ;(add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))
                        ;; lines indentation space-after-tab space-before-tab


;; third-party packages

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters
  :ensure t)



;; (use-package avy
;;   :ensure t
;;   :bind (("s-." . avy-goto-word-or-subword-1)
;;          ("s-," . avy-goto-char))
;;   :config
;;   (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-c C-g g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("C-c C-g t" . git-timemachine)))

(use-package ag
  :ensure t)

;; (use-package projectile
;;   :ensure t
;;   :init
;;   (setq projectile-completion-system 'ivy)
;;   :config
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (projectile-mode +1))

;; (use-package pt
;;   :ensure t)

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))


(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; (use-package easy-kill
;;   :ensure t
;;   :config
;;   (global-set-key [remap kill-ring-save] 'easy-kill))




(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; (use-package clojure-mode
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   (add-hook 'clojure-mode-hook #'subword-mode)
;;   (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; (use-package cider
;;   :ensure t
;;   :config
;;   (setq nrepl-log-messages t)
;;   (add-hook 'cider-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook #'paredit-mode)
;;   (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; (use-package flycheck-joker
;;   :ensure t)

;; (use-package elixir-mode
;;   :ensure t
;;   :config
;;   (add-hook 'elixir-mode #'subword-mode))

;; (use-package erlang
;;   :ensure t
;;   :config
;;   (when (eq system-type 'windows-nt)
;;     (setq erlang-root-dir "C:/Program Files/erl7.2")
;;     (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

;; (use-package haskell-mode
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode #'subword-mode))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

;; (use-package hl-todo
;;   :ensure t
;;   :config
;;   (setq hl-todo-highlight-punctuation ":")
;;   (global-hl-todo-mode)
;;   :bind (("C-c T p" . hl-todo-previous)
;; 		 ("C-c T n" . hl-todo-next)
;; 		 ("C-c T o" . hl-todo-occur)))

;; (use-package zop-to-char
;;   :ensure t
;;   :bind (("M-z" . zop-up-to-char)
;;          ("M-Z" . zop-to-char)))

;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-c i" . imenu-anywhere)
;;          ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode))
  ;(add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; (use-package super-save
;;   :ensure t
;;   :config
;;   ;; add integration with ace-window
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   (super-save-mode +1))

;; (use-package crux
;;   :ensure t
;;   :bind (("C-c o" . crux-open-with)
;;          ("M-o" . crux-smart-open-line)
;;          ("C-c n" . crux-cleanup-buffer-or-region)
;;          ("C-c f" . crux-recentf-find-file)
;;          ("C-M-z" . crux-indent-defun)
;;          ("C-c u" . crux-view-url)
;;          ("C-c e" . crux-eval-and-replace)
;;          ("C-c w" . crux-swap-windows)
;;          ("C-c D" . crux-delete-file-and-buffer)
;;          ("C-c r" . crux-rename-buffer-and-file)
;;          ("C-c t" . crux-visit-term-buffer)
;;          ("C-c k" . crux-kill-other-buffers)
;;          ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
;;          ("C-c I" . crux-find-user-init-file)
;;          ("C-c S" . crux-find-shell-init-file)
;;          ("s-r" . crux-recentf-find-file)
;;          ("s-j" . crux-top-join-line)
;;          ("C-^" . crux-top-join-line)
;;          ("s-k" . crux-kill-whole-line)
;;          ("C-<backspace>" . crux-kill-line-backwards)
;;          ("s-o" . crux-smart-open-line-above)
;;          ([remap move-beginning-of-line] . crux-move-beginning-of-line)
;;          ([(shift return)] . crux-smart-open-line)
;;          ([(control shift return)] . crux-smart-open-line-above)
;;          ([remap kill-whole-line] . crux-kill-whole-line)
;;          ("C-c s" . crux-ispell-word-then-abbrev)))

;; (use-package diff-hl
;;   :ensure t
;;   :config
;;   (global-diff-hl-mode +1)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   ;; autosave the undo-tree history
;;   (setq undo-tree-history-directory-alist
;;         `((".*" . ,temporary-file-directory)))
;;   (setq undo-tree-auto-save-history t))


;; ;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume))

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (global-set-key "\C-s" 'swiper))

;; (use-package counsel
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c a") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;;; my-init.el ends here
