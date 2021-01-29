;;;;;;;;;; delight
(use-package delight
  :ensure t
  )

;;;;;;;;;; try
;; (use-package try
;;  :ensure t)

;;;;;;;;;; which-key
(use-package which-key
  :ensure t
  :delight
  :config
  (which-key-mode 1)
  )

;;;;;;;;;; highlight / hl
(use-package hl-line
  :config
  (global-hl-line-mode 1)
  (set-face-background hl-line-face "gray15"))

(use-package hl-todo
  :ensure t
  :bind (("M-g T" . hl-todo-previous)
         ("M-g t" . hl-todo-next)
         ("M-g M-t" . hl-todo-occur))
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"    . "#FF0000")
          ("FIXME"   . "#F2AF00")
          ("BUG"     . "#A020F0")
          ("REFACTO" . "#FF4500")
          ("DELETE"  . "#1E90FF")))
  ;; (add-hook 'prog-mode-hook #'hl-todo-mode 1)
  ;; (add-hook 'text-mode-hook #'hl-todo-mode 1)
  )

;;;;;;;;;; comment
(use-package comment-dwim-2
  :ensure t
  :bind (("M-#" . comment-dwim-2)
         )
  )

;;;;;;;;;; expand-region
(use-package expand-region
  :ensure t
  :bind (("M-+" . er/expand-region))
  )

;;;;;;;;;; yasnippet
(use-package yasnippet
  :ensure t
  :delight yas-minor-mode
  :bind (("M-_ y n" . yas-new-snippet)
         ("M-_ y i" . yas-insert-snippet)
         ("M-_ y v" . yas-visit-snippet-file))
  :config
  (yas-global-mode 1)
  )
(use-package yasnippet-snippets
  :ensure t
  )
