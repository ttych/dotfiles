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

;;;;;;;;;; ace-window
(use-package ace-window
  :ensure t
  :bind ([remap other-window] . ace-window)
  :config
  (setq aw-ignore-current t
        aw-scope 'frame)
  )

;;;;;;;;;; ag
(use-package ag
  :ensure t
  :bind (("M-s a a" . ag)
         ("M-s a f" . ag-files)
         ("M-s a r" . ag-regexp)
         ("M-s a p" . ag-project)
         ("M-s a F" . ag-project-files)
         ("M-s a R" . ag-project-regexp))
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-window 't)
  )

;;;;;;;;;; anzu
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode)
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

;;;;;;;;;; grep-a-lot
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;;;;;;;;;; htmlize
(use-package htmlize
  :ensure t
  )

;;;;;;;;;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (
         ("M-_ c e" . mc/edit-lines)
         ("M-_ c n" . mc/mark-next-like-this)
         ("M-_ c p" . mc/mark-previous-like-this)
         ("M-_ c w" . mc/mark-next-like-this-word)
         ("M-_ c W" . mc/mark-previous-like-this-word)
         ("M-_ c s" . mc/mark-next-like-this-word)
         ("M-_ c S" . mc/mark-previous-like-this-word)
         ("M-_ c a" . mc/mark-all-like-this)
         )
  )

;;;;;;;;;; paren
(use-package paren
  :config
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  )

;;;;;;;;;; rainbow-mode
(use-package rainbow-mode
  :ensure t
  :delight
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'text-mode-hook 'rainbow-mode)
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

;;;;;;;;;; zoom-window
(use-package zoom-window
  :ensure t
  :bind (
         ("M-_ M-z" . zoom-window-zoom)
         )
  :config
  (custom-set-variables
   '(zoom-window-mode-line-color "DarkGreen"))  ;; Darkblue
  )
