
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
