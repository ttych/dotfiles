;;; init.el --- base emacs configuration

;;; Commentary:

;;; Code:

;; debug
;; (setq debug-on-error t)

(defvar emacs-working-dir (getenv "EMACS_WORKING_DIR"))
(unless (and emacs-working-dir (not (string-equal emacs-working-dir "")))
  (setq emacs-working-dir user-emacs-directory))

(defvar my-vendor-dir (expand-file-name "packages/" emacs-working-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(unless (file-exists-p my-vendor-dir)
  (make-directory my-vendor-dir))
(add-to-list 'load-path my-vendor-dir)

(defvar save-dir (expand-file-name "save/" emacs-working-dir)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p save-dir)
  (make-directory save-dir))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-verbose nil)
 '(auto-save-default t)
 '(auto-save-interval 300)
 '(auto-save-visited-mode nil)
 '(backup-by-copying t)
 '(backward-delete-char-untabify-method nil)
 '(blink-cursor-mode t)
 '(calendar-week-start-day 1)
 '(clean-buffer-list-delay-general 1)
 '(column-number-mode t)
 '(default-frame-alist (quote ((width . 80) (height . 45))))
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(delete-trailing-lines t)
 '(delete-trailing-whitespace t)
 '(gc-cons-threshold 50000000)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-display-line-numbers-mode t)
 '(global-font-lock-mode t)
 '(history-length 1000)
 '(icomplete-mode t)
 '(indent-tabs-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message "")
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((top . 10) (left . 30) (width . 90) (height . 50))))
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message nil)
 '(kept-new-versions 10)
 '(kept-old-versions 2)
 '(large-file-warning-threshold 100000000)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(make-backup-files t)
 '(max-lisp-eval-depth 2000)
 '(menu-bar-mode nil)
 '(next-line-add-newlines nil)
 '(org-babel-load-languages (quote ((emacs-lisp . t))))
 '(read-quoted-char-radix 16)
 '(require-final-newline t)
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-always-indent (quote complete))
 '(tab-width 8)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(vc-make-backup-files t)
 '(version-control t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq european-calendar-style 't)

;; backup and auto-save dir
(setq backup-directory-alist
      `(("." . ,save-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,save-dir t)))

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

(defvar users-settings-dir (expand-file-name "users/" user-emacs-directory)
  "This folder stores user specific setting.")
(defvar user-settings-file
  (expand-file-name (concat user-login-name ".el")
                    users-settings-dir))
(if (file-exists-p user-settings-file)
    (load user-settings-file))


;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;; (defalias 'yes-or-no-p 'y-or-n-p)


;; (defconst *is-unix* (member system-type '(freebsd)))
;; (defconst *is-a-mac* (eq system-type 'darwin))
;; (defconst *is-linux* (member system-type '(gnu gnu/linux gnu/kfreebsd)))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))


;;;;;;;;;; daemon

(defun client-save-kill-emacs()
  " This is a function that can bu used to shutdown save buffers and
	shutdown the emacs daemon. It should be called using
	emacsclient -e '(client-save-kill-emacs)'.  This function will
	check to see if there are any modified buffers or active clients
	or frame.  If so an x window will be opened and the user will
	be prompted."

  (let (new-frame modified-buffers active-clients-or-frames)

    ;; Check if there are modified buffers or active clients or frames.
    (setq modified-buffers (modified-buffers-exist))
    (setq active-clients-or-frames ( or (> (length server-clients) 1)
					(> (length (frame-list)) 1)
					))

    ;; When displaying the number of clients and frames:
    ;; subtract 1 from the clients for this client.
    ;; subtract 2 from the frames this frame (that we just created) and the default frame.
    (when ( or (not active-clients-or-frames)
	       (yes-or-no-p (format "There are currently %d clients and %d frames. Exit anyway?" (- (length server-clients) 1) (- (length (frame-list)) 2))))

      ;; If the user quits during the save dialog then don't exit emacs.
      ;; Still close the terminal though.
      (let((inhibit-quit t))
	;; Save buffers
	(with-local-quit
	  (save-some-buffers))

	(if quit-flag
	    (setq quit-flag nil)
	  ;; Kill all remaining clients
	  (progn
	    (dolist (client server-clients)
	      (server-delete-client client))
	    ;; Exit emacs
	    (kill-emacs)))
	))
    )
  )

(defun modified-buffers-exist()
  "This function will check to see if there are any buffers
	that have been modified.  It will return true if there are
	and nil otherwise. Buffers that have buffer-offer-save set to
	nil are ignored."
  (let (modified-found)
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
		 (buffer-modified-p buffer)
		 (not (buffer-base-buffer buffer))
		 (or
		  (buffer-file-name buffer)
		  (progn
		    (set-buffer buffer)
		    (and buffer-offer-save (> (buffer-size) 0))))
		 )
	(setq modified-found t)
	)
      )
    modified-found
    )
  )


;;;;;;;;;; package
(require 'package)
(setq package-enable-at-startup nil)
;; [Enter ↵] (package-menu-describe-package) → Describe the package under cursor.
;; [i] (package-menu-mark-install) → mark for installation.
;; [u] (package-menu-mark-unmark) → unmark.
;; [d] (package-menu-mark-delete) → mark for deletion (removal of a installed package).
;; [x] (package-menu-execute) → for “execute” (start install/uninstall of marked items).
;; [r] (package-menu-refresh) → refresh the list from server.
;; (For complete list of keys, call describe-mode [Ctrl+h m])
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
	  Your version of Emacs does not support SSL connections,
	  which is unsafe because it allows man-in-the-middle attacks.
	  There are two things you can do about this warning:
	  1. Install an Emacs version that does support SSL and be safe.
	  2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq package-user-dir (expand-file-name "elpa" emacs-working-dir))

(unless package-archive-contents
  (package-refresh-contents))
(global-set-key (kbd "C-x P") 'list-packages)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

;;;;;;;;;; myinit.org
(defvar myinit-org
  (expand-file-name "myinit.org" user-emacs-directory))
(if (file-exists-p myinit-org)
    (org-babel-load-file myinit-org))

;;;;;;;;;; macros
(defvar macros
  (expand-file-name "macros" user-emacs-directory))
(if (file-exists-p macros)
    (load-file macros))

;;; init.el ends here
