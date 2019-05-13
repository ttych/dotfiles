;;; (derived-mode-parents 'dog)
(defun derived-mode-parents (mode)
  (and mode
       (cons mode (derived-mode-parents
                   (get mode 'derived-mode-parent)))))
;; Example :
; (derived-mode-parents 'html-mode)
; (derived-mode-parents 'ruby-mode)
; (derived-mode-parents 'python-mode)
; (derived-mode-parents 'lisp-mode)
