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
  ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;;; NEVER CHANGE THIS !
;; (setq package-user-dir (expand-file-name "elpa" emacs-working-dir))

(unless package-archive-contents
  (package-refresh-contents))
(global-set-key (kbd "C-x P") 'list-packages)


;;;;;;;;;; use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
