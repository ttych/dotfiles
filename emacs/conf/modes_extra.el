
;;;;;;;;;; adoc
(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'"
  )

;;;;;;;;;; cask
(use-package cask-mode
  :ensure t
  )

;;;;;;;;;; clojure
;; (use-package clojure-mode
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   (add-hook 'clojure-mode-hook #'subword-mode)
;;   (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
;;   )

;;;;;;;;;; cucumber
(use-package feature-mode
  :ensure t
  :mode "\\.feature$"
  )

;;;;;;;;;; docbook
(use-package docbook
  :ensure t
  )

;;;;;;;;;; elixir
;; (use-package elixir-mode
;;   :ensure t
;;   :config
;;   (add-hook 'elixir-mode #'subword-mode))

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

;;;;;;;;;; haskel
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode #'subword-mode)
  )

;;;;;;;;;; lisp
(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
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

;;;;;;;;;; org
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )
;; https://github.com/DarkBuffalo/ox-report
(use-package ox-report
  :ensure t
  )
;; (use-package ox-reveal
;;   :ensure t
;;   :config
;;   (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/")
;;   (setq org-reveal-mathjax t)
;;   )

;;;;;;;;;; puppet
(use-package puppet-mode
  :ensure t
  )

;;;;;;;;;; python
;; (use-package py-autopep8
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;   )

;;;;;;;;;; ruby
(use-package haml-mode
  :ensure t
  )
(use-package slim-mode
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
