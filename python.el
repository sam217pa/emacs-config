;;; Configuration pour python.
;;;

(use-package color-identifiers-mode
  ;; colore les variables dans certains mode de programmation
  ;; par une couleur unique
  :ensure t
  :diminish (color-identifiers-mode . "")
  :init
  (add-hook 'python-mode-hook 'color-identifiers-mode))

(use-package python :defer t
  :ensure python-mode
  :mode (("\\.py\\'" . python-mode)
         ("\\.wsgi$" . python-mode))
  :interpreter (("ipython" . python-mode)
                ("python"  . python-mode))
  :config
  (setq-default indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'color-identifiers-mode)
  ;; (X) (add-hook 'python-mode-hook 'python-mode)

  (use-package elpy :ensure t :defer t
    :init
    (add-hook 'python-mode-hook 'elpy-mode)
    :config (electric-indent-local-mode -1))

  (use-package anaconda-mode :ensure t :defer t
    :init
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    :config
    (use-package pyenv-mode :ensure t
      :init (pyenv-mode)))
  )
