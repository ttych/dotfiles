;;;

;;; Commentary:

;;; Code:

;;==================================================
;; Shell mode
;;==================================================

(add-hook
 'term-mode-hook
 (lambda()
   (setq-local show-trailing-whitespace nil)
   (hl-line-mode nil)
   (display-line-numbers-mode -1)
   (linum-mode -1)
   ))

(add-hook
 'eshell-mode-hook
 (lambda()
   (setq-local show-trailing-whitespace nil)
   (hl-line-mode nil)
   (display-line-numbers-mode -1)
   (linum-mode -1)
   ))


;; (defun comint-delchar-or-eof-or-kill-buffer (arg)
;;   (interactive "p")
;;   (if (null (get-buffer-process (current-buffer)))
;;       (kill-buffer)
;;     (comint-delchar-or-maybe-eof arg)))

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (define-key shell-mode-map
;;               (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (define-key shell-mode-map
;;               (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))


;;==================================================
;; org
;;==================================================

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "M-* o l") 'org-store-link)
(global-set-key (kbd "M-* o a") 'org-agenda)
(global-set-key (kbd "M-* o c") 'org-capture)


;;==================================================
;; Bindings
;;==================================================

;; buffers
;; (defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "C-x B") 'ibuffer)
;; (global-set-key (kbd "C-x B") 'ibuffer-other-window)


;; Comment
(global-set-key (kbd "C-c #") 'comment-region)
(global-set-key (kbd "C-c @") 'uncomment-region)
(global-set-key (kbd "M-* #") 'comment-region)
(global-set-key (kbd "M-* @") 'uncomment-region)


;; Search
(global-set-key (kbd "M-s O") 'multi-occur)
(global-set-key (kbd "M-s d") 'find-dired)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s r") 'query-replace-regexp)


;; Modes
(global-set-key (kbd "M-* m f") 'auto-fill-mode)
(global-set-key (kbd "M-* m w") 'whitespace-mode)
(global-set-key (kbd "M-* m s") 'auto-save-mode)
(global-set-key (kbd "M-* m l") 'display-line-numbers-mode)


;; Align
(global-set-key (kbd "C-x \\") 'align-regexp)
(global-set-key (kbd "M-* \\") 'align-regexp)


; C-c C-r    =>  recentf
; C-c C-g g  => magit-status
; C-c C-g t  => git-timemachine



;; Indent region
;(global-set-key (kbd "C-c TAB") 'indent-region)

;; Time
;(global-set-key (kbd "C-c t") 'do_insert_time)

;; Speedbar
;(global-set-key (kbd "C-c s") 'speedbar)

;; goto
;(global-set-key (kbd "C-l") 'goto-line)

;;; Compare windows
;(global-set-key "\C-cw" ’compare-windows)
;(global-set-key (kbd "C-c w") 'compare-windows)

;;; Keybinding for ‘occur’
;(global-set-key (kbd "C-c o") 'occur)

;;; unbind ‘C-x f’ set=fill-column
;(global-unset-key "\C-xf")

;;; Rebind ‘C-x C-b’ for ‘buffer-menu’
;(global-set-key "\C-x\C-b" 'buffer-menu)


(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
