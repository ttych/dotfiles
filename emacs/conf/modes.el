
;;;;;;;;;; adoc
(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'"
  )

;;;;;;;;;; cask
(use-package cask-mode
  :ensure t
  )

;;;;;;;;;; cucumber
(use-package feature-mode
  :ensure t
  :mode "\\.feature$"
  )

;;;;;;;;;; docbook
(use-package docbook
  :ensure t
  )

;;;;;;;;;; emmet
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook  #'emmet-mode)
  )

;;;;;;;;;; git
(use-package magit
  :ensure t
  :bind (("M-* g g" . magit-status))
  )
(use-package git-timemachine
  :ensure t
  :bind (("M-* g t" . git-timemachine))
  )

;;;;;;;;;; groovy
(use-package groovy-mode
  :ensure t
  )

;;;;;;;;;; lua
(use-package lua-mode
  :ensure t
  )

;;;;;;;;;; makefile
(add-hook
 'makefile-mode-hook
 (lambda()
   (setq indent-tabs-mode t
         tab-width 4)
   )
 )

;;;;;;;;;; markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.m[k]d\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t)
  )

;;;;;;;;;; puppet
(use-package puppet-mode
  :ensure t
  )

;;;;;;;;;; terraform
(use-package terraform-mode
  :ensure t
  :config
  (custom-set-variables
   '(terraform-indent-level 2))
  )

;;;;;;;;;; tuareg
(use-package tuareg
  :ensure t
  :mode ("\\.ml[ily]?$" . tuareg-mode)
  )

;;;;;;;;;; yaml
(use-package yaml-mode
  :ensure t
  )
