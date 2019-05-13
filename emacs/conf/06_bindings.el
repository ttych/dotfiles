;;;

;;; Commentary:

;;; Code:

;;; Search
(global-set-key (kbd "M-s O") 'multi-occur)
(global-set-key (kbd "M-s d") 'find-dired)
(global-set-key (kbd "M-s g") 'rgrep)
(global-set-key (kbd "M-s r") 'query-replace-regexp)


;;; Modes
(global-set-key (kbd "C-c m f") 'auto-fill-mode)
(global-set-key (kbd "C-c m w") 'whitespace-mode)
(global-set-key (kbd "C-c m s") 'auto-save-mode)
(global-set-key (kbd "C-c m l") 'display-line-numbers-mode)


;;; Comment
(global-set-key (kbd "C-c #") 'comment-region)
(global-set-key (kbd "C-c @") 'uncomment-region)


;;; Align
(global-set-key (kbd "C-x \\") 'align-regexp)

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
