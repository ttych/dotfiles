;;; init.el --- base emacs configuration

;;; Commentary:

;;; Code:

;; debug
; (setq debug-on-error t)


(defvar my-vendor-dir (expand-file-name "packages/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(unless (file-exists-p my-vendor-dir)
  (make-directory my-vendor-dir))
(add-to-list 'load-path my-vendor-dir)

(defvar savefile-dir (expand-file-name "savefile/" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(backward-delete-char-untabify-method nil)
 '(clean-buffer-list-delay-general 1)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(delete-trailing-lines t)
 '(delete-trailing-whitespace t)
 '(gc-cons-threshold 50000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(history-length 1000)
 '(icomplete-mode t)
 '(indent-tabs-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold 100000000)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(max-lisp-eval-depth 2000)
 '(next-line-add-newlines nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t))))
 '(package-selected-packages
   (quote
    (undo-tree yasnippet-snippets yaml-mode which-key volatile-highlights use-package tuareg try restclient-test rainbow-mode rainbow-delimiters py-autopep8 puppet-mode paredit ox-reveal org-bullets multiple-cursors multi-term move-text magit lua-mode lsp-ui jedi iy-go-to-char inf-ruby htmlize gruvbox-theme groovy-mode grep-a-lot git-timemachine flycheck-inline expand-region emmet-mode emamux elisp-slime-nav delight counsel company-quickhelp company-lsp company-jedi cask-mode anzu ag adoc-mode ace-window ace-jump-mode)))
 '(read-quoted-char-radix 16)
 '(require-final-newline t)
 '(save-place-mode t)
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 8)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-language-environment 'utf-8)

(defun load-directory (dir)
  (let ((load-it
         (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
	(mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory (expand-file-name "conf/" user-emacs-directory))

(org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory))

;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
