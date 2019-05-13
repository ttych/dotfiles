
;;==================================================
;; Editing
;;==================================================
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)


;;==================================================
;; ORG mode
;;==================================================
;; ; TODO sequence with \C-c \C-t
;; ;(setq org-todo-keywords
;; ;      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
;; ;(setq org-todo-keywords
;; ;      '((sequence "TODO(t)" "PENDING(w@/!)" "VERIFY" "|" "DONE(d@/!)" "DELEGATED(D@/!)")
;; ;	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
;; ;	(sequence "|" "CANCELED(c@)")))
;; (setq org-todo-keywords
;;       '((sequence "TODO" "IN-PROGRESS" "WAITING" "TEST" "|" "DELEGATED" "DONE")))

;; ; log - time tracking
;; (setq org-log-done 'time)
;; ;(setq org-log-done 'note)
;; ; adding special markers ‘!’ (for a timestamp) and ‘@’ (for a note) in parentheses after each keyword

;; ;; auto-fill comments but not code
;; (defun comment-auto-fill ()
;;       (setq-local comment-auto-fill-only-comments t)
;;       (auto-fill-mode 1))


;; ;;==============================================================================
;; ;; TEXT
;; ;;==============================================================================
(add-hook 'text-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (auto-fill-mode t)
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
               ; (when (y-or-n-p "Auto Fill mode? ")
               ; (turn-on-auto-fill))
            ))


;;==============================================================================
;; PROGRAMMING
;;==============================================================================
(add-hook 'prog-mode-hook
          (lambda()
            ;(subword-mode 1) ;; move by subword
            (show-paren-mode 1)
            (hl-line-mode 1)
            (whitespace-mode)

            ;(comment-auto-fill)

            ;(electric-indent-mode 1) ; auto indent

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
                  )))


;; 2 - SPACES - INDENTED - MODES
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

;; 4 - SPACES - INDENTED - MODES
(defvar 4-spaces-indented-modes
  '(python-mode
    groovy-mode
    ))
(dolist (mode 4-spaces-indented-modes)
  (add-hook (intern (format "%s-hook" mode))
            (lambda ()
              (setq indent-tabs-mode nil
                    tab-width 4
                    )
              )))

;; TAB - INDENTED - MODES
;; (defvar 4-spaces-indented-modes
;;   '(
;;     ))
;; (dolist (mode space-indented-modes)
;;   (add-hook (intern (format "%s-hook" mode))
;;             (lambda ()
;;               (setq indent-tabs-mode t
;;                     tab-width 4
;;                     )
;;               )))

;; CLEAN - WHITESPACE - MODES



;; Save save-no-trailing-whitespace
;; (defvar save-no-trailing-whitespace
;;   '(
;; 	c-mode-common
;; 	c-mode
;; 	sh-mode
;; 	shell-script-mode
;; 	asm-mode
;; 	lisp-mode
;; 	emacs-lisp-mode
;; 	ruby-mode
;; 	perl-mode
;; 	cperl-mode
;; 	python-mode
;; 	makefile-mode
;;     yaml-mode
;;     haskell-mode
;;     literate-haskell-mode))
;; (dolist (mode save-no-trailing-whitespace)
;;   (add-hook (intern (format "%s-hook" mode))
;;             (lambda ()
;; 			  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
;; 			  )))


;; ;;------------------------------------------------------------------------------
;; ;; C family common settings
;; ;;------------------------------------------------------------------------------
;; ;; cc-mode hooks in order:
;; ;; 1. c-initialization-hook, init cc mode once per session (i.e. emacs startup)
;; ;; 2. c-mode-common-hook, run immediately before loading language hook
;; ;; 3. then language hooks:
;; ;;    c, c++, objc, java, idl, pike, awk
;; (defun my-c-indent ()
;;   (setq
;;    ;; set correct backspace behaviour
;;    ;; c-backspace-function 'backward-delete-char
;;    ;; c-type lang specifics. want 4-space width tab tabs
;;    c-basic-offset 4
;;    c-indent-tabs-mode t               ; tabs please (change t to nil for spaces)
;;    c-indent-level 4
;;    c-tab-always-indent t
;;    tab-width 4
;;    ;; use tabs, not spaces.
;;    indent-tabs-mode t))

;; (add-hook 'c-initialization-hook
;;           (lambda ()
;;             (my-c-indent)            ; just to be sure
;;             (add-to-list 'c-cleanup-list 'comment-close-slash)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (my-c-indent)
;;             ;; subword editing and movement to deal with CamelCase
;;             (c-toggle-electric-state 1)
;;             (subword-mode 1)
;;             (c-toggle-auto-newline 1)
;;             ;; don't indent curly braces. gnu style is madness.
;;             (c-set-offset 'statement-case-open 0)
;;             (c-set-offset 'substatement-open 0)
;;             (c-set-offset 'comment-intro 0)))

;; (autoload 'ac-c-headers "ac-c-headers")
;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (my-c-indent)
;;             (add-to-list 'ac-sources 'ac-source-c-headers)
;;             (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

;; ;;------------------------------------------------------------------------------
;; ;; Assembly
;; ;;------------------------------------------------------------------------------
;; (add-hook 'asm-mode-hook
;;           (lambda ()
;;             (auto-complete-mode 0)
;;             (setq-local asm-comment-char ?\!)
;;             (setq-local tab-width 8)
;;             (setq-local tab-stop-list (number-sequence 8 120 8))
;;             (setq-local indent-tabs-mode t)
;; 			))
