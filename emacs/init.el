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

(defvar users-settings-dir (expand-file-name "users/" user-emacs-directory)
  "This folder stores user specific setting.")
(defvar user-settings-file
  (expand-file-name (concat user-login-name ".el")
                    users-settings-dir))
(if (file-exists-p user-settings-file)
    (load user-settings-file))


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
 '(large-file-warning-threshold 100000000)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(max-lisp-eval-depth 2000)
 '(next-line-add-newlines nil)
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

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; (if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message ""
      inhibit-startup-buffer-menu t
      initial-scratch-message nil)

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

;;; init.el ends here
