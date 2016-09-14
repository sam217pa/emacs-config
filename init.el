;; -*- emacs-lisp -*-
;;; Package.el
(setq gc-cons-threshold 8000000) ; augmente la taille du garbage collector
(package-initialize t)
(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'use-package)

(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; bootstrap `quelpa'
(use-package quelpa :ensure t
  :config
  (setq quelpa-update-melpa-p nil)
  (use-package quelpa-use-package :ensure t))


(use-package general :ensure t)
(use-package diminish)
(use-package bind-key)
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; met en place le serveur pour emacsclient
;;(unless (server-running-p) (server-start))

;;; Sane default
(setq
 use-package-verbose nil  ; use-package décrit les appels qu'il fait
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
 default-fill-column 70
 initial-scratch-message ""
 save-interprogram-paste-before-kill t
 help-window-select t			; focus help window when opened
 )

(prefer-coding-system 'utf-8)           ; utf-8 est le systeme par défaut.

(defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n
(show-paren-mode) ; highlight delimiters
(line-number-mode) ; display line number in mode line
(column-number-mode) ; display colum number in mode line
(save-place-mode)    ; save cursor position between sessions
(setq initial-major-mode 'fundamental-mode)
;; supprime les caractères en trop en sauvegardant.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; rend les scripts executable par défault si c'est un script.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; keybindings

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



;;; -------------------------------------------------------------------
;;; Packages

;;; -A-
(use-package abbrev :defer t
  :diminish "α"
  :init
  (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
  ;; tell emacs where to read abbrev definitions from...
  (setq abbrev-file-name "~/dotfile/emacs/.abbrev_defs")
  (setq save-abbrevs 'silently))

(use-package ace-window :ensure t
  :commands
  ace-window
  :config
  (progn
    (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
    )
  )

(use-package ag :ensure t
  :commands (counsel-ag
	     ag)
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (add-to-list 'ag-arguments "--word-regexp")))

(use-package auctex :ensure t :defer t)

(use-package auto-fill-mode
  :diminish auto-fill-function
  :commands turn-on-auto-fill
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (diminish 'auto-fill-function ""))

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
  :commands aggressive-indent-mode
  :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  :config (progn
            (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
            (add-to-list 'aggressive-indent-excluded-modes 'perl-mode))
  )

;;; -B-
(use-package blank-mode :ensure t
  :commands blank-mode)


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
  ;; to make the byte compiler happy.
  ;; emacs25 has no color-themes variable
  (setq color-themes '())
  :config
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

  (solarized-switch-to-dark))

(use-package command-log-mode :ensure t
  :commands clm/open-command-load-buffer)

(use-package company :ensure t
  :diminish ""
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)

  :config
  (global-company-mode)

  (use-package company-flx :ensure t
    :config
    (company-flx-mode +1))

  (progn
    (setq company-idle-delay 0.5
          company-selection-wrap-around t)
    (define-key company-active-map [tab] 'company-complete)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous))

  (setq company-backends
        '((company-css
	   company-clang
	   company-xcode
	   company-cmake
	   company-capf
	   company-files
	   company-gtags
	   company-etags
	   company-keywords))) )

(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-s"     . counsel-grep-or-swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)
   ("M-/"     . counsel-company))
  )

(use-package counsel-osx-app :ensure t
  :commands counsel-osx-app
  :bind*
  ("C-c a" . counsel-osx-app)
  :config
  (setq counsel-osx-app-location
        '("/Applications/" "~/Applications/" "~/sam_app/"))
  )

(use-package css-mode :ensure t
  :mode (("\\.css\\'" . css-mode)))

;;; -D-
(use-package dired-x
  :defer t
  :init
  (add-hook 'dired-load-hook
            (function (lambda () (load "dired-x"))))

  :config
  ;; use GNU ls instead of BSD ls
  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
	(setq insert-directory-program gls)))
  ;; change default arguments to ls. must include -l
  (setq dired-listing-switches "-XGalg --human-readable --dired"))

(use-package display-time
  :commands
  display-time-mode
  :config
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-format))

;;; -E-
(use-package eldoc :ensure t
  :commands turn-on-eldoc-mode
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  )

(use-package emacs-lisp-mode
  :mode
  (("*scratch*" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)))

  (general-define-key
   :states '(normal emacs)
   :keymaps 'emacs-lisp-mode-map
   :prefix "ê"
    "" '(:ignore t :which-key "Emacs Help")
    "f" 'counsel-describe-function
    "k" 'counsel-descbinds
    "v" 'counsel-describe-variable
    "e" 'eval-last-sexp
    "b" 'eval-buffer
    "c" '(sam--eval-current-form-sp :which-key "eval-current")
    "u" 'use-package-jump
    "t" '(lispy-goto :which-key "goto tag"))
  )

(use-package ereader :ensure t
  :mode (("\\.epub\\'" . ereader-mode)))

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

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'ess-mode-map
   :prefix ","
   :non-normal-prefix "’"               ; Alt-, => ’
    "/" '(:ignore t :which-key "search")
    "," 'ess-handy-commands

    "d" '(:ignore t :which-key "doc")
    "dd" 'ess-help
    "dw" 'ess-help-web-search

    "i" '(:ignore t :which-key "info")
    "id" 'ess-rdired


    "l" '(:ignore t :which-key "load")
    "ll" 'ess-load-library
    "lf" 'ess-load-file

    "s" '(:ignore t :which-key "shell - send")
    "sl"   '(:ignore t :which-key "line")
    "sll"   'ess-eval-line
    "slg"   'ess-eval-line-and-go
    "sls"   'ess-eval-line-and-step
    "sp"   '(:ignore t :which-key "chunk")
    "spp"   'ess-eval-chunk
    "spg"   'ess-eval-chunk-and-go
    "sps"   'ess-eval-chunk-and-step
    "st"   '(:ignore t :which-key "function or §")
    "stt"   'ess-eval-function-or-paragraph
    "stg"   'ess-eval-function-and-go
    "sts"   'ess-eval-function-or-paragraph-and-step
    "sr"   '(:ignore t :which-key "region")
    "srr"  'ess-eval-region
    "srg"  'ess-eval-region-and-go
    "srs"  'ess-eval-region-or-line-and-step

    "w" '(ess-execute-screen-options :which-key "set width")
    )

  ;; secondary major mode map
  (general-define-key
   :states '(normal visual insert)
   :keymaps 'ess-mode-map
   :prefix "ê"
   :non-normal-prefix "’"               ; Alt-, => ’
    "l" 'ess-eval-region-or-line-and-step
    )

  ;; primary map, direct access
  (general-define-key
   :states '(normal insert)
   :keymaps 'ess-mode-map
    "C-RET" 'ess-eval-region-or-line-and-step
    "M-RET" 'ess-eval-function-or-paragraph-and-step
    )

  (setq ess-completing-read 'ivy-completing-read)

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
  (setq ess-offset-continued 2          ; offset after first statement
        ess-expression-offset 2         ; offset for expression
        ess-nuke-trailing-whitespace-p t ;delete trailing whitespace
        ess-default-style 'DEFAULT)      ; set default style for R source file

  (add-to-list
   'aggressive-indent-dont-indent-if    ; do not indent line if
   '(and (derived-mode-p 'ess-mode)     ; in ess mode
         (null (string-match "\\(#+ .+ $\\)" ; and in a roxygen block
                             (thing-at-point 'line)))))


  (sp-local-pair 'ess-mode "%" "%")
  ;; when pressed RET after { or (,
  ;; {
  ;;    | <- cursor
  ;; }
  (sp-local-pair 'ess-mode "{" nil
		 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'ess-mode "(" nil
		 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))

  )

(use-package esup :ensure t
  :commands esup
  )

(use-package evil :ensure t
  ;; change la couleur des curseurs
  :init
  (setq evil-want-fine-undo t)
  (setq evil-want-C-i-jump nil)
  (setq evil-disable-insert-state-bindings t)

  :config
  (evil-mode 1)

  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'message-mode 'motion)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'ivy-occur-mode 'emacs)

  (use-package evil-escape :ensure t
    :diminish
    (evil-escape-mode)
    :config
    (evil-escape-mode)
    (setq-default evil-escape-key-sequence "xq"
                  evil-escape-delay 0.2)
    (setq evil-escape-unordered-key-sequence t))

  (use-package evil-matchit :ensure t
    :commands
    evilmi-jump-items
    :config
    (global-evil-matchit-mode 1))

  (use-package evil-surround :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-visual-mark-mode :ensure t
    :config
    (evil-visual-mark-mode))

  (use-package evil-visualstar :ensure t
    :config
    (global-evil-visualstar-mode t))

  (setq evil-insert-state-cursor  '("#268bd2" bar)  ;; blue
        evil-normal-state-cursor  '("#b58900" box)  ;; blue
        evil-visual-state-cursor  '("#cb4b16" box)  ;; orange
        evil-replace-state-cursor '("#859900" hbar) ;; green
	evil-emacs-state-cursor   '("#d33682" box)) ;; magenta
  )


(use-package exec-path-from-shell :ensure t
  :defer 5
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region :ensure t :defer t)

;;; -F-
(use-package fasd :ensure t
  :config
  (global-fasd-mode 1)
  (setq fasd-completing-read-function 'ivy-completing-read)
  (setq fasd-enable-initial-prompt nil))

(use-package flx :ensure t)

(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
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
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")

  :config
  (add-to-list 'git-gutter:update-commands 'other-window)
  (add-to-list 'git-gutter:update-commands 'save-buffer)

  (use-package git-gutter-fringe :ensure t
    :init
    ;; (setq-default left-fringe-width 10)
    (setq-default right-fringe-width 0)
    :config
    (set-face-foreground 'git-gutter-fr:modified "#268bd2") ;blue
    (set-face-foreground 'git-gutter-fr:added "#859900")    ;green
    (set-face-foreground 'git-gutter-fr:deleted "#dc322f")  ;red

    (fringe-helper-define 'git-gutter-fr:added nil
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      )
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      )
    (fringe-helper-define 'git-gutter-fr:modified nil
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      "..XX......"
      )))

(use-package goto-chg :ensure t
  :commands (goto-last-change
             goto-last-change-reverse)
  )

(use-package grab-mac-link :ensure t
  :commands grab-mac-link)

;;; -H-
(use-package helm :ensure t :disabled t)

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

(use-package hydra :ensure t
  ;; pour les keybindings de fou
  :config
  (use-package ivy-hydra :ensure t)
  )

(use-package hy-mode :ensure t
  :mode (("\\.hy\\'" . hy-mode))
  :init
  (add-hook 'hy-mode-hook (lambda () (lispy-mode 1))))

;;; -I-
(use-package ibuffer :ensure t
  :commands ibuffer)

(use-package ivy
  :quelpa (ivy :fetcher github :repo "abo-abo/swiper")
  :diminish (ivy-mode . "")
  :commands ivy-switch-buffer
  :bind (:map ivy-mode-map
	 ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
	'((nil . ivy--regex-fuzzy)
	  (t   . ivy--regex-ignore-order)))
  )

;;; -J-

;;; -K-

;;; -L-
(use-package linum :defer t
  :init
  (add-hook 'linum-mode-hook 'sam--fix-linum-size)
  )

(use-package lispy :ensure t
  :diminish (lispy-mode . "λ")
  :commands lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))

  :config
  ;; adapté au bépo
  (lispy-define-key lispy-mode-map "s" 'lispy-up)
  (lispy-define-key lispy-mode-map "ß" 'lispy-move-up)
  (lispy-define-key lispy-mode-map "t" 'lispy-down)
  (lispy-define-key lispy-mode-map "þ" 'lispy-move-down)
  (lispy-define-key lispy-mode-map "c" 'lispy-left)
  (lispy-define-key lispy-mode-map "h" 'lispy-clone)
  (lispy-define-key lispy-mode-map "r" 'lispy-right)
  (lispy-define-key lispy-mode-map "®" 'lispy-forward)
  (lispy-define-key lispy-mode-map "©" 'lispy-backward)
  (lispy-define-key lispy-mode-map "C" 'lispy-ace-symbol-replace)
  (lispy-define-key lispy-mode-map "H" 'lispy-convolute)
  (lispy-define-key lispy-mode-map "»" 'lispy-slurp)
  (lispy-define-key lispy-mode-map "«" 'lispy-barf)
  (lispy-define-key lispy-mode-map "l" 'lispy-raise) ; replace "r" `lispy-raise' with "l"
  (lispy-define-key lispy-mode-map "j" 'lispy-teleport)


  (general-define-key
   :states 'insert
   :keymaps 'emacs-lisp-mode-map
    "ê" 'hydra-lispy/body)

  (defhydra hydra-lispy (:hint nil)
    "
       ^ ^          ^ ^ _ß_: mv up   ^ ^  ^ ^       | _l_: raise
       ^ ^          ^ ^ ^|^          ^ ^  ^ ^       | _h_: clone
       ^ ^          ^ ^ _s_: up      ^ ^  ^ ^       | _j_: teleport
       ^ ^          ^ ^ ^|^          ^ ^  ^ ^       |
  bwd: _C_ <- left: _c_ ^+^ _r_: right -> _R_: fwd  |
       ^ ^          ^ ^ ^|^          ^ ^  ^ ^       |
       ^ ^          ^ ^ _t_: down    ^ ^  ^ ^       |
       _b_: back    ^ ^ ^|^          ^ ^  _f_: flow |
       ^ ^          ^ ^ _þ_: mv dn   ^ ^  ^ ^       |
  "
    ("s" lispy-up)
    ("ß" lispy-move-up)
    ("t" lispy-down)
    ("þ" lispy-move-down)
    ("c" lispy-left)
    ("r" lispy-right)
    ("h" lispy-clone)
    ("C" lispy-backward)
    ("R" lispy-forward)
    ("l" lispy-raise)
    ("j" lispy-teleport)
    ("f" lispy-flow)
    ("b" lispy-back))

  ;; change avy-keys to default bépo home row keys.
  (setq lispy-avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m)))

(use-package lorem-ipsum :ensure t
  :commands
  (lorem-ipsum-insert-list
   lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraph)
  )

;;; -M-
(use-package magit :ensure t
  :commands
  (magit-blame
   magit-commit
   magit-commit-popup
   magit-diff-popup
   magit-diff-unstaged
   magit-fetch-popup
   magit-init
   magit-log-popup
   magit-pull-popup
   magit-push-popup
   magit-revert
   magit-stage-file
   magit-status
   magit-unstage-file
   magit-blame-mode)

  :config
  (global-git-commit-mode)

  (use-package git-commit :ensure t :defer t)

  (use-package magit-gitflow :ensure t
    :commands
    turn-on-magit-gitflow
    :general
    (:keymaps 'magit-mode-map
     "%" 'magit-gitflow-popup)
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

  (use-package git-messenger :ensure t
    :general
    (:keymaps 'git-messenger-map
     "q" 'git-messenger:popup-close))

  (use-package git-timemachine :ensure t
    :commands git-timemachine
    :general
    (:keymaps 'git-timemachine-mode-map
     "n" 'git-timemachine-show-next-revision
     "p" 'git-timemachine-show-previous-revision
     "q" 'git-timemachine-quit
     "w" 'git-timemachine-kill-abbreviated-revision
     "W" 'git-timemachine-kill-revision))

  (setq magit-completing-read-function 'ivy-completing-read))

(use-package makefile-mode :defer t
  :init
  (add-hook 'makefile-bsdmake-mode-hook 'makefile-gmake-mode)
  )

(use-package markdown-mode :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config

  (defhydra hydra-markdown (:hint nil)
    "
Formatting         _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code
Headings           _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4
Lists              _m_: insert item
Demote/Promote     _l_: promote       _r_: demote     _U_: move up      _D_: move down
Links, footnotes   _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference
undo               _u_: undo
"


    ("s" markdown-insert-bold)
    ("e" markdown-insert-italic)
    ("b" markdown-insert-blockquote :color blue)
    ("p" markdown-insert-pre :color blue)
    ("c" markdown-insert-code)

    ("h" markdown-insert-header-dwim)
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)

    ("m" markdown-insert-list-item)

    ("l" markdown-promote)
    ("r" markdown-demote)
    ("D" markdown-move-down)
    ("U" markdown-move-up)

    ("L" markdown-insert-link :color blue)
    ("U" markdown-insert-uri :color blue)
    ("F" markdown-insert-footnote :color blue)
    ("W" markdown-insert-wiki-link :color blue)
    ("R" markdown-insert-reference-link-dwim :color blue)

    ("u" undo :color teal)
    )

  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'markdown-mode-map
   :prefix ","
   :non-normal-prefix "’"               ; Alt-, => ’
    "," 'hydra-markdown/body
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
(use-package nlinum :ensure t
  :commands nlinum-mode)

;;; -O-

(use-package osx-clipboard :ensure t
  :if (not (window-system))
  :init
  (osx-clipboard-mode +1))

(use-package outline
  :defer t
  :diminish ((outline-minor-mode . "")
	     (outline-major-mode . "")))

;;; -P-
(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages)
  )

(use-package pbcopy :ensure t
  :if (not (display-graphic-p))
  :init
  (turn-on-pbcopy))

(use-package prettify-symbols-mode
  :init
  (global-prettify-symbols-mode))

(use-package pretty-mode :ensure t
  :disabled t
  :commands turn-on-pretty-mode
  :init
  (add-hook 'ess-mode-hook 'turn-on-pretty-mode)
  )

(use-package projectile :ensure t
  :diminish (projectile-mode . "ⓟ")
  :commands
  (projectile-ag
   projectile-switch-to-buffer
   projectile-invalidate-cache
   projectile-find-dir
   projectile-find-file
   projectile-find-file-dwim
   projectile-find-file-in-directory
   projectile-ibuffer
   projectile-kill-buffers
   projectile-kill-buffers
   projectile-multi-occur
   projectile-multi-occur
   projectile-switch-project
   projectile-switch-project
   projectile-switch-project
   projectile-recentf
   projectile-remove-known-project
   projectile-cleanup-known-projects
   projectile-cache-current-file
   projectile-project-root
   )
  :config
  (projectile-global-mode 1)

  (use-package counsel-projectile :ensure t)

  (progn
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
(use-package rainbow-delimiters  :ensure t
  :commands rainbow-delimiters-mode
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
  :config
  (setq recentf-max-saved-items 50)
  )

(use-package restart-emacs :ensure t
  :commands restart-emacs
  )

;;; -S-
(use-package scss-mode :ensure t
  :mode ("\\.scss\\'" . scss-mode))

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
  :commands smartparens-global-mode
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
  (add-hook 'after-init-hook (lambda () (smartparens-global-mode)))
  ;; (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  ;; (add-hook 'org-mode-hook #'smartparens-mode)
  ;; (add-hook 'ess-mode-hook #'smartparens-mode)

  :config
  (progn
    ;; Only use smartparens in web-mode
    (sp-local-pair 'web-mode "<% " " %>")
    (sp-local-pair 'web-mode "{ " " }")
    (sp-local-pair 'web-mode "<%= "  " %>")
    (sp-local-pair 'web-mode "<%# "  " %>")
    (sp-local-pair 'web-mode "<%$ "  " %>")
    (sp-local-pair 'web-mode "<%@ "  " %>")
    (sp-local-pair 'web-mode "<%: "  " %>")
    (sp-local-pair 'web-mode "{{ "  " }}")
    (sp-local-pair 'web-mode "{% "  " %}")
    (sp-local-pair 'web-mode "{%- "  " %}")
    (sp-local-pair 'web-mode "{# "  " #}")
    (sp-local-pair 'org-mode "$" "$")
    (sp-pair "'" nil :actions :rem))


  (defun sam--create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    ;; from [Newline and indent on appropriate pairs · Issue #80 · ;; Fuco1/smartparens](https://github.com/Fuco1/smartparens/issues/80)
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  )

(use-package smex
  :quelpa (smex :fetcher github :repo "abo-abo/smex"))

(use-package spaceline :ensure t
  :defer 1
  :config
  (use-package spaceline-config
    :init
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (setq powerline-default-separator 'utf-8)

    :config
    (spaceline-emacs-theme)))

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
(use-package wgrep :ensure t :defer t)

(use-package which-key :ensure t
  :diminish
  which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (setq which-key-popup-type 'side-window
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.05)
  )

(use-package whitespace
  :diminish ""
  :commands whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail))
  )

(use-package web-mode :ensure t
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode)))

;;; -X-

;;; -Y-
(use-package yasnippet
  :diminish yas-minor-mode
  :defer 10
  :init
  (with-eval-after-load 'yasnippet
    (progn
      (setq yas-snippet-dirs (append yas-snippet-dirs
				     '("~/dotfile/emacs/snippets")))))
  :config
  (yas-global-mode)
  (setq yas-indent-line nil))

;;; -Z-
;;; -------------------------------------------------------------------

;;; personal functions
(load-file "~/dotfile/emacs/functions.el")
;;; keybindings
(load-file "~/dotfile/emacs/keybindings.el")
;;; org
(load-file "~/dotfile/emacs/org.el")

;;; custom
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
