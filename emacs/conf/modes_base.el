;;;;;;;;;; indentation
;; 2 SPACES - INDENTED - MODES
(defvar 2-spaces-indented-modes
  '(ruby-mode
    html-mode
    yaml-mode
    ))
(dolist (mode 2-spaces-indented-modes)
  (add-hook (intern (format "%s-hook" mode))
            (lambda ()
              (setq indent-tabs-mode nil
                    tab-width 2
                    )
              )))

;; 4 SPACES - INDENTED - MODES
(defvar 4-spaces-indented-modes
  '(python-mode
    groovy-mode
    markdown-mode
    ))
(dolist (mode 4-spaces-indented-modes)
  (add-hook (intern (format "%s-hook" mode))
            (lambda ()
              (setq indent-tabs-mode nil
                    tab-width 4
                    )
              )))

;; TAB 4 - INDENTED - MODES
(defvar tab-indented-modes
  '(
    ))
(dolist (mode tab-indented-modes)
  (add-hook (intern (format "%s-hook" mode))
            (lambda ()
              (setq indent-tabs-mode t
                    tab-width 4
                    )
              )))

;;;;;;;;;; lisp
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
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

(add-hook 'ielm-mode-hook #'eldoc-mode)
(add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)


;;;;;;;;;; org <M-_ o>
(global-set-key (kbd "M-_ o l") 'org-store-link)
(global-set-key (kbd "M-_ o a") 'org-agenda)
(global-set-key (kbd "M-_ o c") 'org-capture)
;; adding special markers ‘!’ (for a timestamp) and ‘@’ (for a note) in parentheses after each keyword
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "BACKLOG(b!)" "READY(r!)" "WIP(w!)" "BLOCKED(B@)" "|" "DONE(d!)" "CANCELLED(c@)"))
      )
;; (setq org-log-done 'time)
;; (setq org-log-done 'note)

;;;;;;;;;; prog-mode
(add-hook 'prog-mode-hook
          (lambda()
            ;; (subword-mode 1) ;; move by subword
            (show-paren-mode 1)
            (hl-line-mode 1)
            (whitespace-mode)

            ;; (comment-auto-fill)
            ;; (electric-indent-mode 1) ; auto indent

            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
                                        ;(add-to-list 'write-file-functions 'whitespace-cleanup)

            (setq delete-trailing-lines t
                  indent-tabs-mode nil
                  tab-width 4
                  show-paren-delay 0
                  comment-multi-line t
                  whitespace-line-column 80
                  ;; whitespace-style '(face trailing tab-mark lines-tail)
                  ;; whitespace-display-mappings
                  ;; '(
                  ;; 	(tab-mark 9 [9655 9] [92 9]) ; tab  “▷”
                  ;; 	(newline-mark 10 [182 10]) ; LINE FEED “¶”
                  ;; 						;(space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」w
                  ;; 	)
                  ))
          )

;;;;;;;;;; shell
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

(add-hook
 'shell-mode-hook
 (lambda()
   (setq-local show-trailing-whitespace nil)
   (hl-line-mode nil)
   (display-line-numbers-mode -1)
   (linum-mode -1)
   ))

(add-to-list 'auto-mode-alist '("\\.shl\\'" . shell-script-mode))

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;;;;;;;;; text-mode
(add-hook 'text-mode-hook
          (lambda ()
            (turn-on-visual-line-mode)
            (turn-on-auto-fill)
            (setq
             ;; use tabs
             indent-tabs-mode t
             ;; tabs size is 4 spaces
             tab-width 4
             ;; default insert is also 4 and inc of 4
             ;; got to specify this or it will continue to expand to 8 spc
             tab-stop-list (number-sequence 4 120 4)
             )
            ;; ask to turn on hard line wrapping
            ;; (when (y-or-n-p "Auto Fill mode? ")
            ;; (turn-on-auto-fill))
            )
          )

;;;;;;;;;; whitespace
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'whitespace-mode))
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))
