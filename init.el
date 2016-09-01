;; -*- emacs-lisp -*-

(setq gc-cons-threshold 2000000) ; augmente la taille du garbage collector

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(exec-path-from-shell-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package general :ensure t)
(require 'diminish)
(require 'bind-key)
(require 'server)

;; met en place le serveur pour emacsclient
(unless (server-running-p) (server-start))

(setq
 use-package-verbose t  ; use-package décrit les appels qu'il fait
 delete-old-versions -1	; supprime les vieilles versions des fichiers sauvegardés
 version-control t	; enable le version control
 vc-make-backup-files t	; backups file even when under vc
 backup-directory-alist `(("." . "~/.emacs.d/backups")) ; central dir for backups
 vc-follow-symlinks t	; vc suit les liens  symboliques
 auto-save-file-name-transforms
 '((".*" "~/.emacs.d/auto-save-list/" t)); transforme les noms des fichiers sauvegardés
 inhibit-startup-screen t ; supprime l'écran d'accueil
 ring-bell-function 'ignore ; supprime cette putain de cloche.
 coding-system-for-read 'utf-8          ; use UTF8 pour tous les fichiers
 coding-system-for-write 'utf-8         ; idem
 sentence-end-double-space nil          ; sentences does not end with double space.
 default-fill-column 80
 initial-scratch-message ""
 save-interprogram-paste-before-kill t
 )



;; (setq select-enable-clipboard nil)

(prefer-coding-system 'utf-8)           ; utf-8 est le systeme par défaut.

;; apparences
(when window-system
  (tooltip-mode -1) ; don't know what that is
  (tool-bar-mode -1) ; sans barre d'outil
  (menu-bar-mode 1) ; barre de menu
  (scroll-bar-mode -1) ; enlève la barre de défilement
  (set-frame-font "Fira Code 13") ; police par défault
  (blink-cursor-mode -1) ; pas de clignotement
  (global-visual-line-mode)
  (diminish 'visual-line-mode "") )

;; change la police par défault pour la frame courante et les futures.
(add-to-list 'default-frame-alist '(font . "Fira Code Light 13"))
(set-face-attribute 'default nil :font "Fira Code Light 13")

;;;
;;; keybindings
;;;

(when (eq system-type 'darwin)	; mac specific bindings
  (setq mac-right-command-modifier 'meta ; cmd de droite = meta
	mac-command-modifier 'control ; cmd de gauche = control
	mac-option-modifier 'super ; option de gauche = super
	mac-right-option-modifier nil ; option de droite = carac spéciaux
	mac-control-modifier 'hyper ; control de gauche = control
	ns-function-modifier 'hyper ; fn key = hyper
	ns-right-alternate-modifier nil); cette touche n'existe pas.
  (setq locate-command "mdfind")
  )
;;;
;;; global default mode
;;;

(defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n
(show-paren-mode) ; highlight delimiters
(line-number-mode) ; display line number in mode line
(column-number-mode) ; display colum number in mode line
(setq fill-column 80) ; élargit un peu la largeur de texte par défault.
(setq initial-major-mode 'fundamental-mode)
;; supprime les caractères en trop en sauvegardant.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; rend les scripts executable par défault si c'est un script.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; -------------------------------------------------------------------
;;;
;;; Packages
;;;
;; -------------------------------------------------------------------

;;; -A-
(use-package ace-window :ensure t
  :commands
  ace-window
  :config
  (progn
    (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
    )
  )

(use-package ag :ensure t :disabled t
  :config (progn
            (setq ag-highlight-search t)
            (setq ag-reuse-buffers t)
            (add-to-list 'ag-arguments "--word-regexp"))
  )

(use-package auctex :ensure t :defer t)

(use-package auto-fill
  :diminish auto-fill-mode
  :commands turn-on-auto-fill
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  )

(use-package autorevert :defer t
  :diminish auto-revert-mode
  ;; mainly to make autorevert disappear from the modeline
  )

(use-package avy :ensure t :defer t
  :config
  (setq avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m))
  )

(use-package aggressive-indent :ensure t
  :diminish (aggressive-indent-mode . "")
  :init (global-aggressive-indent-mode 1)
  :config (progn
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'perl-mode))
  )

;;; -B-
(use-package blank-mode :ensure t
  :commands blank-mode)

(use-package browse-kill-ring :ensure t
  :commands browse-kill-ring
  :diminish ""
  :config
  (setq browse-kill-ring-separator "———")
  )

;;; -C-

(use-package color-identifiers-mode :ensure t
  ;; colore les variables dans certains mode de programmation
  ;; par une couleur unique
  :commands color-identifiers-mode
  :diminish (color-identifiers-mode . "")
  :init
  (add-hook 'python-mode-hook 'color-identifiers-mode)
  )

(use-package color-theme-solarized :ensure t
  :init
  ;; to make the byte compiler happy. emacs25 has no color-themes variable
  (setq color-themes '())
  ;; load the theme, don't ask for confirmation
  (load-theme 'solarized t)
  (defun solarized-switch-to-dark ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'dark)
    (enable-theme 'solarized))
  (defun solarized-switch-to-light ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'light)
    (enable-theme 'solarized))

  (solarized-switch-to-dark)
  )

(use-package company :ensure t :defer t
  :diminish ""
  :init
  (global-company-mode)
  :config
  (progn
    (setq company-idle-delay 0.2
          company-selection-wrap-around t)
    (define-key company-active-map [tab] 'company-complete)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))
  )

(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  )

(use-package counsel-osx-app :ensure t
  :commands counsel-osx-app
  :bind*
  ("C-c a" . counsel-osx-app)
  :config
  (setq counsel-osx-app-location
        '("/Applications/" "~/Applications/" "~/sam_app/"))
  )

;;; -D-
(use-package dired-x
  :defer t
  :init
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

  :config
                                        ; use GNU ls instead of BSD ls
  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
	(setq insert-directory-program gls)))

                                        ; change default arguments to ls. must include -l
  (setq dired-listing-switches "-XGalg --human-readable --dired")
  )

(use-package display-time
  :commands
  display-time-mode
  :config
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-format)
  )

;;; -E-
(use-package ess-site
  :ensure ess
  :mode
  (("\\.sp\\'"           . S-mode)
   ("/R/.*\\.q\\'"       . R-mode)
   ("\\.[qsS]\\'"        . S-mode)
   ("\\.ssc\\'"          . S-mode)
   ("\\.SSC\\'"          . S-mode)
   ("\\.[rR]\\'"         . R-mode)
   ("\\.[rR]nw\\'"       . Rnw-mode)
   ("\\.[sS]nw\\'"       . Snw-mode)
   ("\\.[rR]profile\\'"  . R-mode)
   ("NAMESPACE\\'"       . R-mode)
   ("CITATION\\'"        . R-mode)
   ("\\.omg\\'"          . omegahat-mode)
   ("\\.hat\\'"          . omegahat-mode)
   ("\\.lsp\\'"          . XLS-mode)
   ("\\.do\\'"           . STA-mode)
   ("\\.ado\\'"          . STA-mode)
   ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
   ("\\.jl\\'"           . ess-julia-mode)
   ("\\.[Ss]t\\'"        . S-transcript-mode)
   ("\\.Sout"            . S-transcript-mode)
   ("\\.[Rr]out"         . R-transcript-mode)
   ("\\.Rd\\'"           . Rd-mode)
   ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
   ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))

  :commands
  (R stata julia SAS)

  :init
  (add-hook 'ess-mode-hook 'company-mode)
  (add-hook 'ess-mode-hook
            (lambda () (flycheck-mode)
              (run-hooks 'prog-mode-hook 'company-mode-hook)))
  (add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))

  :config
  (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t)
                                   (ess-R-fl-keyword:%op% . t)))
  (setq ess-offset-continued 2
        ess-expression-offset 2
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT)

  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'ess-mode)
         (null (string-match "\\(#+ .+ $\\)"
                             (thing-at-point 'line)))))

  )

(use-package evil :ensure t
  :init
  (evil-mode 1)
  ;; change la couleur des curseurs
  :config

  (use-package evil-escape :ensure t
    :diminish
    (evil-escape-mode)
    :init
    (evil-escape-mode)
    :config
    (setq-default evil-escape-key-sequence "xq"
                  evil-escape-delay 0.2)
    (setq evil-escape-unordered-key-sequence t)
    )

  (use-package evil-matchit :ensure t
    :commands
    evilmi-jump-items
    :init
    (global-evil-matchit-mode 1)
    )

  (use-package evil-surround :ensure t
    :init
    (global-evil-surround-mode)
    )

  (use-package evil-visualstar :ensure t
    :init
    (global-evil-visualstar-mode t)
    )

  (setq evil-insert-state-cursor  '("#268bd2" bar) ;; blue
        evil-normal-state-cursor  '("#b58900" box) ;; blue
        evil-visual-state-cursor  '("#cb4b16" box) ;; orange
        evil-replace-state-cursor '("#859900" hbar) ;; green
        evil-emacs-state-cursor   '("#d33682" box)) ;; magenta

  ;;première étape: avant de réaffecter c,t,s,r en h,j,k,l, il faut
  ;;retirer ces touches de l’agencement de clavier normal-state
  (define-key evil-normal-state-map "c" nil)
  (define-key evil-normal-state-map "C" nil)
  (define-key evil-normal-state-map "s" nil)
  (define-key evil-normal-state-map "S" nil)
  (define-key evil-normal-state-map "r" nil)
  (define-key evil-normal-state-map "R" nil)
  (define-key evil-normal-state-map "j" nil)
  (define-key evil-normal-state-map "J" nil)
  ;;je redéfinis certaines fonctions pour l’état normal
  (define-key evil-normal-state-map "h" 'evil-change)
  (define-key evil-normal-state-map "H" 'evil-change-line)
  (define-key evil-normal-state-map "T" 'evil-join)
  (define-key evil-normal-state-map "l" 'evil-replace)
  (define-key evil-normal-state-map "L" 'evil-replace-state)
  (define-key evil-normal-state-map "k" 'evil-substitute)
  (define-key evil-normal-state-map "K" 'evil-change-whole-line)
  ;;même chose mais cette fois pour l’état motion
  (define-key evil-motion-state-map "c" 'evil-backward-char)
  (define-key evil-motion-state-map "C" 'evil-window-top)
  (define-key evil-motion-state-map "t" 'evil-next-line)
  (define-key evil-motion-state-map "s" 'evil-previous-line)
  (define-key evil-motion-state-map "r" 'evil-forward-char)
  (define-key evil-motion-state-map "R" 'evil-window-bottom)
  (define-key evil-motion-state-map "j" 'evil-find-char-to)
  (define-key evil-motion-state-map "J" 'evil-find-char-to-backward)

  )

(use-package evil-lisp-state :ensure t
  :commands (evil-lisp-state)
  :init
  (add-hook 'lisp-mode-hook (lambda () 'evil-lisp-state))
  (add-hook 'emacs-lisp-mode-hook (lambda () 'evil-lisp-state))
  (add-hook 'clojure-mode-hook (lambda () 'evil-lisp-state))
  :config
  (evil-lisp-state-leader "SPC l")
  )

(use-package eldoc :ensure t :defer t
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))

(use-package emacs-lisp
  :commands emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))
  )

(use-package expand-region :ensure t :defer t)

;;; -F-
(use-package flycheck :ensure t :defer t
  :diminish (flycheck-mode . "ⓕ")
  :init
  (add-hook 'prog-mode-hook  'flycheck-mode)
  :config
  (progn
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc r-lintr))
    (setq flycheck-highlighting-mode 'lines)
    (setq flycheck-check-syntax-automatically '(save)))
  )

;;; -G-
(use-package git-gutter :ensure t
  :diminish ""
  :commands (global-git-gutter-mode)
  :init
  (global-git-gutter-mode +1)
  )

(use-package grab-mac-link :ensure t
  :commands grab-mac-link)

;;; -H-
(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  )

(use-package hl-line
  ;; souligne la ligne du curseur
  :init
  (global-hl-line-mode)
  )

(use-package hydra :ensure t :defer t
  ;; pour les keybindings de fou
  )

;;; -I-
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :init (ivy-mode 1)
  :bind (:map ivy-mode-map
         ("C-'" . ivy-avy))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "(%d/%d) ")
  )

;;; -J-

;;; -K-

;;; -L-
(use-package linum :defer t
  :init
  (add-hook 'linum-mode-hook 'sam--fix-linum-size)
  )

;;; -M-
(use-package magit :ensure t
  :init
  (global-git-commit-mode)

  :commands
  (magit-blame-mode
   magit-commit-popup
   magit-diff-popup
   magit-fetch-popup
   magit-log-popup
   magit-pull-popup
   magit-push-popup
   magit-status)

  :config
  (use-package git-commit :ensure t
    :defer t
    )

  (use-package magit-gitflow :ensure t
    :commands
    turn-on-magit-gitflow
    :general
    (:keymaps 'magit-mode-map
     "%" 'magit-gitflow-popup)
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    )

  (use-package git-messenger :ensure t
    :general
    (:keymaps 'git-messenger-map
     "q" 'git-messenger:popup-close)
    )

  (use-package git-timemachine :ensure t
    :commands git-timemachine
    :general
    (:keymaps 'git-timemachine-mode-map
     "n" 'git-timemachine-show-next-revision
     "p" 'git-timemachine-show-previous-revision
     "q" 'git-timemachine-quit
     "w" 'git-timemachine-kill-abbreviated-revision
     "W" 'git-timemachine-kill-revision)
    )

  (setq magit-completing-read-function 'ivy-completing-read)

  )

(use-package makefile-mode :defer t
  :init
  (add-hook 'makefile-bsdmake-mode-hook 'makefile-gmake-mode)
  )

(use-package markdown-mode :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'markdown-mode-map
   :prefix ","
   :non-normal-prefix "’"               ; Alt-, => ’
    "=" 'markdown-promote
    "°" 'markdown-promote-subtree
    "-" 'markdown-demote
    "8" 'markdown-demote-subtree
    "o" 'markdown-follow-thing-at-point
    "j" 'markdown-jump
    "»" 'markdown-indent-region
    "«" 'markdown-exdent-region
    "gc" 'markdown-forward-same-level
    "gr" 'markdown-backward-same-level
    "gs" 'markdown-up-heading
    )
  )

;;; -N-

;;; -O-

(use-package osx-clipboard :ensure t
  :init
  (osx-clipboard-mode +1)
  )

;;; -P-
(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages)
  )

(use-package pbcopy :ensure t
  :if (not (display-graphic-p))
  :init
  (turn-on-pbcopy))

(use-package projectile :ensure t
  :disabled t
  :defer 10
  :diminish (projectile-mode . "ⓟ")
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :config (progn
            (setq projectile-completion-system 'ivy)
            (add-to-list 'projectile-globally-ignored-files ".DS_Store")))


(use-package python
  :ensure python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.wsgi$" . python-mode))
  :interpreter
  (("ipython" . python-mode)
   ("python"  . python-mode))
  :config

  (use-package elpy :ensure t
    :commands (elpy-mode
               elpy-enable
               )
    :bind* ("M-/" . elpy-company-backend)
    :init
    (add-hook 'python-mode-hook 'elpy-mode)
    :config
    (electric-indent-local-mode -1)

    (general-define-key
     :states '(normal visual insert emacs)
     :keymaps 'python-mode-map
     :prefix ","
     :non-normal-prefix "’"               ; Alt-, => ’
      "/" '(:ignore t :which-key "search")
      "/s" '(elpy-rgrep-symbol :which-key "search symbol")

      "d" '(:ignore t :which-key "doc")
      "dd" 'elpy-doc

      "f" '(:ignore t :which-key "file")
      "ff" 'elpy-find-file

      "g" '(:ignore t :which-key "go to")
      "gd" 'elpy-goto-definition
      "gb" 'pop-tag-mark

      "m" '(:ignore t :which-key "move")
      "mt" 'elpy-nav-move-line-or-region-down
      "ms" 'elpy-nav-move-line-or-region-up
      "mc" 'elpy-nav-indent-shift-left
      "mr" 'elpy-nav-indent-shift-right

      "n" '(:ignore t :which-key "nav")
      "nt" 'elpy-nav-forward-block
      "ns" 'elpy-nav-backward-block
      "nc" 'elpy-nav-backward-indent
      "nr" 'elpy-nav-forward-indent

      "r" '(:ignore t :which-key "refactor")
      "rf" 'elpy-format-code
      "re" 'elpy-multiedit-python-symbol-at-point
      "ri" '(elpy-importmagic-fixup :which-key "import fix")
      "rr" 'elpy-refactor

      "s" '(:ignore t :which-key "shell")
      "sg" 'elpy-shell-switch-to-shell
      "sl" 'elpy-shell-send-current-statement
      "sb" 'elpy-shell-send-region-or-buffer
      "sr" 'elpy-shell-send-region-or-buffer
      "st" 'elpy-shell-send-defun
      "si" 'elpy-use-ipython

      "t" 'elpy-test

      "v" '(:ignore t :which-key "virtualenv")
      "va" 'pyenv-activate
      "vd" 'pyenv-deactivate
      "vo" 'pyenv-workon ; TODO describe this keybinding

      )

    ;; CONTROL like map
    (general-define-key
     :states '(insert)
     :keymaps 'python-mode-map
     :non-normal-prefix "ê"
      "t" 'elpy-nav-forward-block
      "s" 'elpy-nav-backward-block
      "c" 'elpy-nav-backward-indent
      "r" 'elpy-nav-forward-indent

      "f" 'elpy-shell-send-defun
      "RET" 'elpy-shell-send-current-statement
      )

    ;; META like map
    (general-define-key
     :states '(insert)
     :keymaps 'python-mode-map
     :non-normal-prefix "à"
      "t" 'elpy-nav-move-line-or-region-down
      "s" 'elpy-nav-move-line-or-region-up
      "c" 'elpy-nav-indent-shift-left
      "r" 'elpy-nav-indent-shift-right
      )

    )

  (use-package pyenv-mode :ensure t
    :commands pyenv-mode
    :init (add-hook 'python-mode-hook 'pyenv-mode)
    )

  (setq-default indent-tabs-mode nil)
  (setq python-indent-offset 4)
  )

;;; -Q-

;;; -R-
(use-package rainbow-delimiters  :ensure t :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package ranger :ensure t
  :commands (ranger
             deer)
  :bind (("C-x d" . deer))

  :general
  (:keymaps 'ranger-mode-map
	    "t" 'ranger-next-file ; j
	    "s" 'ranger-prev-file ; k
	    "r" 'ranger-find-file ; l
	    "c" 'ranger-up-directory ; c
	    "j" 'ranger-toggle-mark ; t
	    )

  :config
  (setq ranger-cleanup-eagerly t)
  )

(use-package recentf
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  (recentf-mode 1)
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

;;; -S-
(use-package sh-script :defer t
  ;; shell-scripts
  :init
  ;; Use sh-mode when opening `.zsh' files, and when opening
  ;; Prezto runcoms.
  (progn
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))))

(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "")
  :bind (("C-M-f" . sp-forward-sexp)
         ("C-M-b" . sp-backward-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-a" . sp-backward-down-sexp)
         ("C-S-d" . sp-beginning-of-sexp)
         ("C-S-a" . sp-end-of-sexp)
         ("C-M-e" . sp-up-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         )
  :init
  (smartparens-global-mode)

  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'org-mode-hook #'smartparens-mode)
    (add-hook 'ess-mode-hook #'smartparens-mode)
    (sp-local-pair 'org-mode "$" "$")
    (sp-local-pair 'ess-mode "%" "%")
    (sp-pair "'" nil :actions :rem))
  )

(use-package subword :defer t
  :init
  (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))
  :diminish "")

(use-package swiper :ensure t
  :commands swiper
  )

;;; -T-

;;; -U-
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :bind* (("C-x u" . undo-tree-visualize))
  )

;;; -V-
(use-package visual-regexp-steroids :ensure t
  :commands (
             vr/replace
             vr/query-replace
             )
  )

;;; -W-
(use-package which-key :ensure t
  :diminish
  which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-popup-type 'side-window
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.05)
  )

(use-package whitespace
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail))
  )

;;; -X-

;;; -Y-
;; TODO : setup yasnippet
(use-package yasnippet :ensure t
  :disabled t
  :diminish (yas-minor-mode . "")
  :init
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  )

;;; -Y-

;; -------------------------------------------------------------------

;; personal functions
(load-file "~/dotfile/emacs/functions.el")
;; keybindings
(load-file "~/dotfile/emacs/keybindings.el")
;; org
(load-file "~/dotfile/emacs/org.el")

;; custom goes after that
;; -------------------------------------------------------------------
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
