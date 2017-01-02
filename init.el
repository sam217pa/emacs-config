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
(use-package quelpa
  :load-path "~/.emacs.d/private/quelpa"
  :config
  (setq quelpa-update-melpa-p nil)
  (use-package quelpa-use-package
    :load-path "~/.emacs.d/private/quelpa-use-package"))


(use-package general
  :quelpa (general :fetcher github :repo "noctuid/general.el"))
(use-package use-package-chords :config (key-chord-mode 1))
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
 tab-width 4                    ; tab are 4 spaces large
 )

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil
              tab-width 4)

(prefer-coding-system 'utf-8)           ; utf-8 est le systeme par défaut.

(defalias 'yes-or-no-p 'y-or-n-p) ; remplace yes no par y n
(show-paren-mode) ; highlight delimiters
(line-number-mode) ; display line number in mode line
(column-number-mode) ; display colum number in mode line
(save-place-mode)    ; save cursor position between sessions
(delete-selection-mode 1)               ; replace highlighted text with type
(setq initial-major-mode 'fundamental-mode)
;; supprime les caractères en trop en sauvegardant.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; apparences
(when window-system
  (tooltip-mode -1)                    ; don't know what that is
  (tool-bar-mode -1)                   ; sans barre d'outil
  (menu-bar-mode -1)                    ; barre de menu
  (scroll-bar-mode -1)                 ; enlève la barre de défilement
  (set-frame-font "Inconsolata 14")    ; police par défault
  (blink-cursor-mode 1)               ; pas de clignotement
  (global-visual-line-mode)
  (diminish 'visual-line-mode ""))

(when window-system
  (set-frame-size (selected-frame) 85 61))

(setq-default cursor-type 'hbar)
(add-to-list 'default-frame-alist '(cursor-color . "#d33682"))

(add-to-list 'default-frame-alist '(height . 46))
(add-to-list 'default-frame-alist '(width . 85))

;; change la police par défault pour la frame courante et les futures.
(add-to-list 'default-frame-alist '(font . "Source Code Pro 13"))
(set-face-attribute 'default nil :font "Source Code Pro 13")

;; rend les scripts executable par défault si c'est un script.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;;; keybindings

(when (eq system-type 'darwin)           ; mac specific bindings
  (setq mac-right-command-modifier 'meta ; cmd de droite = meta
        mac-command-modifier 'control    ; cmd de gauche = control
        mac-option-modifier 'super       ; option de gauche = super
        mac-right-option-modifier nil ; option de droite = carac spéciaux
        mac-control-modifier 'hyper ; control de gauche = hyper (so does capslock)
        ns-function-modifier 'hyper ; fn key = hyper
        ns-right-alternate-modifier nil) ; cette touche n'existe pas.
  (setq locate-command "mdfind")
  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil nil nil file)))



;;; Packages -------------------------------------------------------------------

;; ---------- A --------------------------------------------------
(use-package abbrev :defer t
  :diminish ""
  :init
  (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
  ;; tell emacs where to read abbrev definitions from...
  (setq abbrev-file-name "~/dotfile/emacs/.abbrev_defs")
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package ace-window :ensure t
  :commands
  ace-window
  :config
  (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
  (setq aw-background t)
  (setq aw-ignore-current t))

(use-package ag :ensure t
  :commands (counsel-ag
             ag)
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers t)
    (add-to-list 'ag-arguments "--word-regexp")))

(use-package all-the-icons )

(use-package anzu :ensure t
  :diminish ""
  :bind*
  (("C-/" . anzu-query-replace-at-cursor)
   ("C-9" . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package auto-fill-mode
  :diminish auto-fill-function
  :commands turn-on-auto-fill
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (diminish 'auto-fill-function ""))

(use-package auto-insert-mode
  :config
  (auto-insert-mode)
  (setq auto-insert-directory "~/dotfile/emacs/autoinsert")
  (define-auto-insert "\\.R\\'" "header.R"))

(use-package autorevert :defer t
  ;; mainly to make autorevert disappear from the modeline
  :diminish auto-revert-mode)

(use-package avy :ensure t
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-1
             avy-goto-char-in-line
             avy-goto-line)
  :config
  (setq avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m))
  (setq avy-styles-alist
        '((avy-goto-char-in-line . post)
          (avy-goto-word-or-subword-1 . post))))

(use-package avy-zap :ensure t
  :bind (("M-Z" . avy-zap-to-char-dwim)
         ("M-z" . avy-zap-up-to-char-dwim)))

(use-package aggressive-indent :ensure t
  :diminish (aggressive-indent-mode . "")
  :commands aggressive-indent-mode
  :init
  (add-hook 'prog-mode-hook 'aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'perl-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'ess-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if    ; do not indent line if
   '(and (derived-mode-p 'ess-mode)     ; in ess mode
         (null (string-match "\\(^#\\+ .+ $\\)" ; and in a roxygen block
                             (thing-at-point 'line))))))

;; ---------- B --------------------------------------------------
(use-package beacon
  :diminish ""
  :quelpa (beacon :fetcher github :repo "Malabarba/beacon")
  :config
  (beacon-mode 1))

(use-package blank-mode :ensure t
  :commands blank-mode)

;; ---------- C --------------------------------------------------
(use-package color-theme-solarized :ensure t
  :disabled t
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
    (enable-theme 'solarized)
    (set-cursor-color "#d33682"))
  (defun solarized-switch-to-light ()
    (interactive)
    (set-frame-parameter nil 'background-mode 'light)
    (enable-theme 'solarized)
    (set-cursor-color "#d33682"))

  (solarized-switch-to-dark))

(use-package command-log-mode :ensure t
  :commands (command-log-mode))

(use-package company :ensure t
  :diminish ""
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t)

  :config
  (global-company-mode)

  (use-package company-statistics
    :quelpa (company-statistics :fetcher github :repo "company-mode/company-statistics")
    :config
    (company-statistics-mode))

  (bind-keys :map company-active-map
    ("C-d" . company-show-doc-buffer)
    ("C-l" . company-show-location)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-t" . company-select-next)
    ("C-s" . company-select-previous)
    ("TAB" . company-complete))

  (setq company-backends
        '((company-css
           company-clang
           company-capf
           company-semantic
           company-xcode
           company-cmake
           company-files
           company-gtags
           company-etags
           company-keywords)))

  ;; from https://github.com/syl20bnr/spacemacs/blob/master/layers/auto-completion/packages.el
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(use-package counsel :ensure t
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f"   . counsel-git)
   ("C-c s"   . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate))
  :config
  (setq counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")

  ;; from http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
  (defun counsel-yank-bash-history ()
    "Yank the bash history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                             (buffer-string))
                           "\n"
                           t)))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Bash history:") collection))))
        (insert val)
        (message "%s => kill-ring" val))))

  ;; TODO make the function respects reverse order of file
  (defun counsel-yank-zsh-history ()
    "Yank the zsh history"
    (interactive)
    (let (hist-cmd collection val)
      (shell-command "history -r")      ; reload history
      (setq collection
            (nreverse
             (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zhistory"))
                                             (buffer-string))
                           "\n"
                           t)))
      (setq collection (mapcar (lambda (it) (replace-regexp-in-string ".*;" "" it)) collection))
      (when (and collection (> (length collection) 0)
                 (setq val (if (= 1 (length collection)) (car collection)
                             (ivy-read (format "Zsh history:") collection :re-builder #'ivy--regex-ignore-order))))
        (kill-new val)
        (insert val)
        (message "%s => kill-ring" val))))

  (defun counsel-package-install ()
    (interactive)
    (ivy-read "Install package: "
              (delq nil
                    (mapcar (lambda (elt)
                              (unless (package-installed-p (car elt))
                                (symbol-name (car elt))))
                            package-archive-contents))
              :action (lambda (x)
                        (package-install (intern x)))
              :caller 'counsel-package-install))
  (ivy-set-actions
   'counsel-find-file
   '(("o" (lambda (x) (counsel-find-file-extern x)) "open extern"))))

(use-package counsel-osx-app :ensure t
  :commands counsel-osx-app
  :bind*
  ("C-c a" . counsel-osx-app)
  :config
  (setq counsel-osx-app-location
        '("/Applications/" "~/Applications/" "~/sam_app/")))

(use-package counsel-gtags :ensure t
  :defer t)

(use-package css-mode :ensure t
  :mode (("\\.css\\'" . css-mode)))

(use-package csv-mode :ensure t
  :mode (("\\.csv\\'" . csv-mode))
  :bind (:map csv-mode-map
         ("," . hydra-csv/body))
  :defines hydra-csv/body
  :config
  (defhydra hydra-csv (:hint nil :color amaranth)
    "
^NAV^        ^TRANSFORM^      ^ALIGN^         ^TOGGLE^         ^YANK^
_f_: fwd     _s_: sort        _a_: align      _d_: desc        _k_: kill
_b_: bwd     _S_: sort num    _A_: unalign    _T_: invisible   _y_: yank
_n_: next    _t_: transpose   ^ ^             ^ ^              _Y_: yank as new table
_p_: prev    _r_: reverse
"
    ("f" csv-forward-field)
    ("b" csv-backward-field)
    ("n" next-line)
    ("p" previous-line)
    ("t" csv-transpose)
    ("s" csv-sort-fields)
    ("S" csv-sort-numeric-fields)
    ("a" csv-align-fields)
    ("A" csv-unalign-fields)
    ("r" csv-reverse-region)
    ("d" csv-toggle-descending)
    ("T" csv-toggle-invisibility)
    ("k" csv-kill-fields)
    ("y" csv-yank-fields)
    ("Y" csv-yank-as-new-table)
    ("u" undo "undo")
    ("q" nil "quit" :color blue)))

;; ---------- D --------------------------------------------------
(use-package debian-control-mode
  :load-path "~/.emacs.d/private/dcf/"
  :commands debian-control-mode
  :mode (("DESCRIPTION" . debian-control-mode)))

(use-package dired
  :commands (dired)
  :config
  (bind-keys :map dired-mode-map
    ("." . hydra-dired-main/body)
    ("t" . dired-next-line)
    ("s" . dired-previous-line)
    ("r" . dired-find-file)
    ("c" . dired-up-directory))

  ;; use GNU ls instead of BSD ls
  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))
  ;; change default arguments to ls. must include -l
  (setq dired-listing-switches "-XGalg --group-directories-first --human-readable --dired")

  (use-package dired-x
    :init
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    :config
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-verbose nil)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

  (use-package dired-details :ensure t
    :config
    (dired-details-install)
    (setq dired-details-hidden-string " + "
          dired-details-hide-link-targets nil)))


(use-package display-time
  :commands
  display-time-mode
  :config
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-format))

;; ---------- E --------------------------------------------------
(use-package ebib
  :quelpa (ebib :fetcher github :repo "joostkremers/ebib")
  :commands (ebib)
  :config

  (general-define-key :keymaps 'ebib-index-mode-map
    "." 'hydra-ebib/body
    "p" 'hydra-ebib/ebib-prev-entry
    "n" 'hydra-ebib/ebib-next-entry
    "C-p" 'hydra-ebib/ebib-push-bibtex-key-and-exit
    "C-n" 'hydra-ebib/ebib-search-next)

  (setq ebib-preload-bib-files '("~/Dropbox/bibliography/stage_m2.bib"
                                 "~/Dropbox/bibliography/Biologie.bib"))

  (setq ebib-notes-use-single-file "~/dotfile/bibliographie/notes.org")

  (defhydra hydra-ebib (:hint nil :color blue)
    "
     ^Nav^            ^Open^           ^Search^        ^Action^
     ^---^            ^----^           ^------^        ^------^
  _n_: next        _e_: entry       _/_: search     _m_: mark
  _p_: prev        _i_: doi       _C-n_: next       _a_: add entry
_M-n_: next db     _u_: url         ^ ^             _r_: reload
_M-p_: prev db     _f_: file        ^ ^           _C-p_: push key
^   ^              _N_: note        ^ ^           ^   ^
"
    ("a" ebib-add-entry)
    ("e" ebib-edit-entry)
    ("f" ebib-view-file)
    ("g" ebib-goto-first-entry)
    ("G" ebib-goto-last-entry)
    ("i" ebib-browse-doi)
    ("m" ebib-mark-entry :color red)
    ("n" ebib-next-entry :color red)
    ("N" ebib-open-note)
    ("C-n" ebib-search-next :color red)
    ("M-n" ebib-next-database :color red)
    ("p" ebib-prev-entry :color red)
    ("C-p" ebib-push-bibtex-key)
    ("M-p" ebib-prev-database :color red)
    ("r" ebib-reload-all-databases :color red)
    ("u" ebib-browse-url)
    ("/" ebib-search :color red)
    ("?" ebib-info)
    ("q" ebib-quit "quit")
    ("." nil "toggle")))

(use-package eldoc :ensure t
  :commands turn-on-eldoc-mode
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))

(use-package emacs-lisp-mode
  :mode
  (("*scratch*" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
              (setq-local outline-regexp ";; ----------\\|^;;;")))

  (general-define-key
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
    "t" '(lispy-goto :which-key "goto tag")))

(use-package ereader :ensure t
  :mode (("\\.epub\\'" . ereader-mode)))

(use-package eshell
  :defines eshell-here
  :commands (eshell
             eshell-here)
  :config
  (use-package eshell-z :ensure t)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

  (setq eshell-directory-name "~/dotfile/emacs/eshell/")

  (general-define-key
   :keymaps 'eshell-mode-map
    "<tab>" (lambda () (interactive) (pcomplete-std-complete))
    "C-'" (lambda () (interactive) (insert "exit") (eshell-send-input) (delete-window))))

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
  (add-hook 'ess-mode-hook (lambda () (lesspy-mode 1)))
  (add-hook 'ess-mode-hook (lambda () (aggressive-indent-mode -1)))
  (add-hook 'ess-mode-hook (lambda () (setq-local outline-regexp "### ----------\\|^##' #")))
  :config
  (load-file "~/dotfile/emacs/ess-config.el"))

(use-package esup :ensure t
  :commands esup)

(use-package exec-path-from-shell :ensure t
  :defer 2
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region :ensure t
  :defines hydra-expand-region/body
  :bind (("C-=" . er/expand-region)
         ("s-=" . hydra-expand-region/er/expand-region)
         ("C-°" . er/contract-region)
         ("s-°" . hydra-expand-region/er/contract-region))
  :general
  ("s-SPC" 'hydra-expand-region/er/expand-region)
  :config
  (setq expand-region-fast-keys-enabled nil)
  (defhydra hydra-expand-region (:color red :hint nil)
    "
^Expand region^   ^Cursors^
_c_: expand       _n_: next
_r_: contract     _p_: prev
_e_: exchange
_R_: reset
"
    ("c" er/expand-region)
    ("r" er/contract-region)
    ("e" exchange-point-and-mark)
    ("R" (lambda () (interactive)
           (let ((current-prefix-arg '(0)))
             (call-interactively #'er/contract-region))) :color blue)
    ("n" hydra-mc/mc/mark-next-like-this :color blue)
    ("p" hydra-mc/mc/mark-previous-like-this :color blue)
    ("q" nil "quit" :color blue)))

;; ---------- F --------------------------------------------------
(use-package fastx
  :load-path "~/.emacs.d/private/fastx"
  :mode (("\\.fasta$" . fastx-mode)
         ("\\.fa$" . fastx-mode)
         ("\\.fst$" . fastx-mode)
         ("\\.fastq$" . fastq-mode)
         ("\\.fq$" . fastq-mode))
  :bind (:map fastx-mode-map
         ("c" . fastx-hide-sequence)
         ("t" . fastx-next-sequence)
         ("s" . fastx-prev-sequence)
         ("r" . fastx-show-sequence)))


(use-package fish-mode :ensure t
  :mode (("\\.fish$" . fish-mode)))

(use-package flx :ensure t)

(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

;; ---------- G --------------------------------------------------
(use-package ggtags :ensure t
  :commands (ggtags-mode)
  :config
  (general-define-key :keymaps 'ggtags-mode-map
    "M-s-," 'ggtags-navigation-mode-abort
    "M-s-." 'ggtags-find-tag-dwim))

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
  (add-to-list 'git-gutter:update-commands 'save-buffer))

(use-package google-this :ensure t
  :bind (("C-x g" . google-this-mode-submap)
         ("C-x G" . google-this)))

(use-package go-mode :ensure t
  :mode (("\\.go\\'" . go-mode))
  :config
  (when (memq window-system '(mac ns x))
    (dolist (var '("GOPATH" "GO15VENDOREXPERIMENT"))
      (unless (getenv var)
        (exec-path-from-shell-copy-env var))))

  (use-package company-go :ensure t
    :config
    (setq company-go-show-annotation t)
    (add-to-list 'company-backends 'company-go))

  (use-package go-eldoc :ensure t
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup))

  (use-package go-guru
    :load-path "~/src/golang.org/x/tools/cmd/guru/"
    :config
    (defhydra hydra-go-guru (:color blue :columns 2)
      ("s" go-guru-set-scope "scope")
      ("cr" go-guru-callers "callers")
      ("ce" go-guru-callees "callees")
      ("P" go-guru-peers "peers")
      ("d" go-guru-definition "def")
      ("f" go-guru-freevars "freevars")
      ("s" go-guru-callstack "stack")
      ("i" go-guru-implements "implements")
      ("p" go-guru-pointsto "points to")
      ("r" go-guru-referrers "referrers")
      ("?" go-guru-describe "describe")))

  (use-package flycheck-gometalinter :ensure t
    :config
    (setq flycheck-disabled-checkers '(go-gofmt go-golint go-vet go-build go-test go-errcheck))
    (flycheck-gometalinter-setup))

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; set tab width
  (add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

  (dolist (delim '("{" "(" "["))
    (sp-local-pair 'go-mode delim nil
                   :post-handlers '((sam--create-newline-and-enter-sexp "RET"))))

  ;; from https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Blang/go/packages.el
  (defun go-run-main ()
    (interactive)
    (async-shell-command
     (format "go run %s"
             (shell-quote-argument (buffer-file-name)))))

  ;; keybindings
  (general-define-key
   :keymaps 'go-mode-map
    "C-," 'hydra-go/body)

  (defhydra hydra-go (:hint nil :color teal)
    "
         ^Command^      ^Imports^       ^Doc^
         ^-------^      ^-------^       ^---^
      _r_: run      _ig_: goto       _d_: doc at point
    [_g_]: guru     _ia_: add
    ^  ^            _ir_: remove
    "
    ("g" hydra-go-guru/body :color blue)
    ("r" go-run-main)
    ("d" godoc-at-point)
    ("ig" go-goto-imports )
    ("ia" go-import-add)
    ("ir" go-remove-unused-imports)
    ("q" nil "quit" :color blue)))

(use-package goto-chg :ensure t
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package grab-mac-link :ensure t
  :commands grab-mac-link)

;; ---------- H --------------------------------------------------
(use-package haskell-mode :ensure t
  :mode (("\\.hs\\'" . haskell-mode))
  :config
  (use-package intero :ensure t
    :init
    (add-hook 'haskell-mode-hook 'intero-mode)
    :commands (intero-mode)
    :diminish " λ"))

(use-package helm
  ;; disabled for now, but I've copy and pasted here the advice from
  ;; tuhdo about helm.
  :disabled t
  :commands (helm-mode)
  :bind (("M-x" . helm-M-x))
  :config
  (setq
   ;; open helm buffer inside current window, not occupy whole other window
   helm-split-window-in-side-p t
   ;; input close to where I type
   helm-echo-input-in-header-line t)

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1))

(use-package hideshow
  :commands hs-minor-mode
  :diminish hs-minor-mode
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package hl-line
  ;; souligne la ligne du curseur
  :init
  (global-hl-line-mode))

(use-package hungry-delete :ensure t
  :diminish ""
  :config
  (global-hungry-delete-mode))

(use-package hydra :ensure t)

(use-package hy-mode :ensure t
  :mode (("\\.hy\\'" . hy-mode))
  :init
  (add-hook 'hy-mode-hook (lambda () (lispy-mode 1))))

;; ---------- I --------------------------------------------------
(use-package ibuffer :ensure t
  :commands ibuffer
  :init
  (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
  :config
  (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body))

(use-package ido
  :defer t
  :config
  (general-define-key :keymaps 'ido-completion-map
    "C-n" 'ido-next-match
    "C-p" 'ido-prev-match))

(use-package iedit :ensure t
  :bind (("C-*" . iedit-mode)))

(use-package imenu-anywhere
  :quelpa (imenu-anywhere :fetcher github :repo "vspinu/imenu-anywhere")
  :commands ivy-imenu-anywhere)

(use-package indent-tools :ensure t
  :bind (("C-c C-i" . indent-tools-hydra/body)))

(use-package "isearch"
  :bind* (("C-s" . isearch-forward)
          ("C-r" . isearch-backward))
  :config
  (defun symbol-name-at-point ()
    (let ((symbol (symbol-at-point)))
      (if symbol (symbol-name symbol) "")))

  (defun current-thing ()
    "Return the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
    (let ((thing (if (region-active-p)
                     (buffer-substring (region-beginning) (region-end))
                   (symbol-name-at-point))))
      (deactivate-mark) thing))

  (defun isearch-thing ()
    "Search for the current \"thing\":
- if the region is active, return the region's text and deactivate the mark
- else return the symbol at point or the empty string."
    (interactive)
    (isearch-yank-string (current-thing)))

  (define-key isearch-mode-map (kbd "C-t") #'isearch-thing))

(use-package ispell :ensure t
  :defines ispell-word-then-abbrev
  :commands (ispell-word-then-abbrev
             ispell)
  :bind* (("s-:" . ispell)
          :map ctl-x-map
          ("C-i" . ispell-word-then-abbrev))
  :config

  (setq ispell-dictionary "francais")

  (defun ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (sam--simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil              ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (sam--simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point")))))

(use-package ivy
  :quelpa (ivy :fetcher github :repo "abo-abo/swiper")
  :diminish (ivy-mode . "")
  :commands (ivy-switch-buffer
             ivy-switch-buffer-other-window)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  ;; from https://github.com/company-mode/company-statistics
  ;; ignore buffers in the ignore buffer list.
  (setq ivy-use-ignore-default 'always)
  (setq ivy-ignore-buffers '("company-statistics-cache.el" "company-statistics-autoload.el"))
  ;; if ivy-flip is t, presents results on top of query.
  (setq ivy-flip nil)
  (setq ivy-overlay-at nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-ignore-order)
          (manual-entry . ivy--regex-ignore-order)
          (t      . ivy--regex)
          (t      . ivy--regex-ignore-order)))

  (defun ivy-switch-project ()
    (interactive)
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name))

  (global-set-key (kbd "C-c m") 'ivy-switch-project)

  (ivy-set-actions
   'ivy-switch-project
   '(("d" dired "Open Dired in project's directory")
     ("v" counsel-projectile "Open project root in vc-dir or magit")
     ("c" projectile-compile-project "Compile project")
     ("r" projectile-remove-known-project "Remove project(s)"))))

;; ---------- J --------------------------------------------------

;; ---------- K --------------------------------------------------

;; ---------- L --------------------------------------------------
(use-package lesspy
  :load-path "~/.emacs.d/private/lesspy"
  :diminish ""
  :commands (lesspy-mode)
  :config
  (general-define-key
   :keymaps 'lesspy-mode-map
    "a" 'lesspy-avy-jump
    "p" 'lesspy-eval-function-or-paragraph
    "h" 'lesspy-help
    "l" 'lesspy-eval-line
    "L" 'lesspy-eval-line-and-go
    "e" 'lesspy-eval-sexp
    "c" 'lesspy-left
    "t" 'lesspy-down
    "s" 'lesspy-up
    "r" 'lesspy-right
    "d" 'lesspy-different
    "m" 'lesspy-mark
    "x" 'lesspy-execute
    "u" 'lesspy-undo
    "z" 'lesspy-to-shell
    "(" 'lesspy-paren
    "»" 'lesspy-forward-slurp
    "«" 'lesspy-backward-slurp
    "#" 'sam--double-hash-at-line-begin
    "'" 'lesspy-roxigen
    "C-d" 'lesspy-delete-forward
    "C-e" 'lesspy-end-of-sexp
    "C-(" 'lesspy-paren-wrap-next
    "DEL" 'lesspy-delete-backward))

(use-package linum :defer t
  :init
  (add-hook 'linum-mode-hook 'sam--fix-linum-size))

(use-package lispy :ensure t
  :diminish (lispy-mode . " λ")
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
  (lispy-define-key lispy-mode-map "X" 'special-lispy-x)

  ;; change avy-keys to default bépo home row keys.
  (setq lispy-avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m)))

(use-package lorem-ipsum :ensure t
  :commands
  (lorem-ipsum-insert-list
   lorem-ipsum-insert-sentences
   lorem-ipsum-insert-paragraphs))

;; ---------- M --------------------------------------------------
(use-package magit :ensure t
  :commands (magit-blame
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
  :bind (("s-v" . magit-status))

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
  ;; (add-hook 'makefile-mode-hook 'makefile-gmake-mode)
  (add-hook 'makefile-bsdmake-mode-hook 'makefile-gmake-mode)
  (add-hook 'makefile-mode-hook (lambda () (aggressive-indent-mode 0)
                                  (setq-local outline-regexp "^## ----------")))
  :config
  (add-to-list 'company-backends 'company-shell))

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("README"   . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 0)))

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
   :keymaps 'markdown-mode-map
   :prefix "C-,"
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
    "gs" 'markdown-up-heading)

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
    "C-c C-a" "insert"
    "C-c C-c" "export"
    "C-c TAB" "images"
    "C-c C-s" "text"
    "C-c C-t" "header"
    "C-c C-x" "move"))

(use-package move-text :ensure t
  :commands
  (move-text-down
   move-text-up))

(use-package multiple-cursors
  :quelpa (multiple-cursors :fetcher github :repo "magnars/multiple-cursors.el")
  :general
  ("M-s-s" 'hydra-mc/mc/mark-previous-like-this
   "M-s-t" 'hydra-mc/mc/mark-next-like-this
   "M-s-S" 'mc/unmark-next-like-this
   "M-s-T" 'mc/unmark-previous-like-this
   "H-SPC" 'hydra-mc/body)
  :commands
  (hydra-mc/mc/mark-previous-like-this
   hydra-mc/mc/mark-next-like-this)
  :config

  ;; from https://github.com/abo-abo/hydra/wiki/multiple-cursors
  (defhydra hydra-mc (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
    ("l" mc/edit-lines :exit t)
    ("a" mc/mark-all-like-this :exit t)
    ("n" mc/mark-next-like-this)
    ("N" mc/skip-to-next-like-this)
    ("M-p" mc/unmark-next-like-this)
    ("p" mc/mark-previous-like-this)
    ("P" mc/skip-to-previous-like-this)
    ("M-n" mc/unmark-previous-like-this)
    ("r" mc/mark-all-in-region-regexp :exit t)
    ("q" nil :color blue)
    (" " nil "quit" :color blue)))

(use-package multi-term :ensure t
  :commands (multi-term
             multi-term-next
             multi-term-prev
             multi-term-dedicated-open
             multi-term-dedicated-close
             multi-term-dedicated-select
             multi-term-dedicated-toggle)
  :init
  (setq multi-term-program "/usr/local/bin/bash"))

;; ---------- N --------------------------------------------------
(use-package nlinum :ensure t
  :commands (global-nlinum-mode
             nlinum-mode)
  :init
  (defun sam--fix-linum-size ()
    "Fixe la taille de charactère dans linum mode"
    (interactive)
    (set-face-attribute 'linum nil :height 100 :foreground "#93a1a1"))
  :config
  (add-hook 'nlinum-mode-hook 'sam--fix-linum-size)
  (global-nlinum-mode))

;; ---------- O --------------------------------------------------
(use-package osx-clipboard :ensure t
  :if (not (window-system))
  :init
  (osx-clipboard-mode +1))

(use-package openwith :ensure t
  :config
  (openwith-mode 1)
  (setq openwith-associations
        '(("\\.pdf\\'" "/Applications/Preview.app/Contents/MacOS/Preview" (file)))))

;; TODO work on outline hydra. useful for tex
(use-package outline
  :bind (("H-<tab>" . hydra-outline/body))
  :commands (outline-hide-body
             outline-show-all
             outline-minor-mode
             hydra-outline/body)
  :general ("C-M-c" 'outline-previous-heading
            "C-M-r" 'outline-next-heading)
  :diminish ((outline-minor-mode . "")
             (outline-major-mode . ""))
  :config
  (outline-minor-mode)
  (defhydra hydra-outline
    (:hint nil :body-pre (outline-minor-mode 1))
    "
Outline

   ^Current^  ^All^            ^Manipulate^
   ^-------^  ^----^           ^----------^
_c_: hide  _C_: hide      _M-r_: demote
_t_: next  ^^             _M-c_: promote
_s_: prev  ^^             _M-t_: move down
_r_: show  _R_: show      _M-s_: move up
"
    ("C" outline-hide-body)
    ("t" outline-next-visible-heading)
    ("s" outline-previous-visible-heading)
    ("R" outline-show-all)
    ("c" outline-hide-subtree)
    ("r" outline-show-subtree)

    ("M-r" outline-demote)
    ("M-c" outline-promote)
    ("M-t" outline-move-subtree-down)
    ("M-s" outline-move-subtree-up)

    ("i" outline-insert-heading "insert heading" :color blue)
    ("q" nil "quit" :color blue)))

;; ---------- P --------------------------------------------------
(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages))

(use-package pbcopy :ensure t
  :if (not (display-graphic-p))
  :init
  (turn-on-pbcopy))

(use-package persp-mode
  :defer t
  :quelpa (persp-mode :fetcher github :repo "Bad-ptr/persp-mode.el")
  :diminish (persp-mode . "")
  :commands (persp-mode
             persp-next
             persp-prev
             pers-switch)
  :config

  (setq wg-morph-on nil)                ; switch off animation ?
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-nil-name "nil")

  (defhydra hydra-persp (:hint nil :color blue)
    "
^Nav^        ^Buffer^      ^Window^     ^Manage^      ^Save/load^
^---^        ^------^      ^------^     ^------^      ^---------^
_n_: next    _a_: add      ^ ^          _r_: rename   _w_: save
_p_: prev    _b_: → to     ^ ^          _c_: copy     _W_: save subset
_s_: → to    _i_: import   _S_: → to    _C_: kill     _l_: load
^ ^          ^ ^           ^ ^          ^ ^           _L_: load subset
"
    ("n" persp-next :color red)
    ("p" persp-prev :color red)
    ("s" persp-switch)
    ("S" persp-window-switch)
    ("r" persp-rename)
    ("c" persp-copy)
    ("C" persp-kill)
    ("a" persp-add-buffer)
    ("b" persp-switch-to-buffer)
    ("i" persp-import-buffers-from)
    ("I" persp-import-win-conf)
    ("o" persp-mode)
    ("w" persp-save-state-to-file)
    ("W" persp-save-to-file-by-names)
    ("l" persp-load-state-from-file)
    ("L" persp-load-from-file-by-names)
    ("q" nil "quit"))

  (global-set-key (kbd "H-p") 'persp-prev)
  (global-set-key (kbd "H-n") 'persp-next))

;; TODO what is that ?
(use-package pretty-mode :ensure t
  :disabled t
  :commands turn-on-pretty-mode
  :init
  (add-hook 'ess-mode-hook 'turn-on-pretty-mode))

(use-package prog-mode
  :config
  (global-prettify-symbols-mode))

(use-package projectile :ensure t
  :defines hydra-projectile/body
  :diminish (projectile-mode . " ⓟ")
  :commands (projectile-ag
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
             projectile-mode
             projectile-project-p)
  :bind ("s-p" . hydra-projectile/body)
  :config
  (projectile-global-mode 1)

  (use-package counsel-projectile :ensure t)

  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (defun hydra-projectile-if-projectile-p ()
    (interactive)
    (if (projectile-project-p)
        (hydra-projectile/body)
      (counsel-projectile)))

  (defhydra hydra-projectile
    (:color teal :hint nil
     :pre (projectile-mode))
    "
     PROJECTILE: %(projectile-project-root)

    ^FIND FILE^        ^SEARCH/TAGS^        ^BUFFERS^       ^CACHE^                    ^PROJECT^
    _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
    _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd      _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
    _r_: recent file   ^ ^                  ^ ^             _z_: cache current
    _d_: dir
"
    ("a"   projectile-ag)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-dir)
    ("f"   projectile-find-file)
    ("F"   projectile-find-file-dwim)
    ("C-f" projectile-find-file-in-directory)
    ("g"   ggtags-update-tags)
    ("s-g" ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("q"   nil "cancel" :color blue)))

(use-package python
  :ensure python-mode
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.pyx\\'" . python-mode)
   ("\\.wsgi$" . python-mode))
  :interpreter
  (("ipython" . python-mode)
   ("python"  . python-mode))

  :config
  (load-file "~/dotfile/emacs/python-config.el"))

;; ---------- Q --------------------------------------------------

;; ---------- R --------------------------------------------------
(use-package rainbow-delimiters  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ranger :ensure t
  :commands
  (ranger
   deer)
  :general
  (:keymaps 'ranger-mode-map
   "t" 'ranger-next-file		; j
   "s" 'ranger-prev-file		; k
   "r" 'ranger-find-file		; l
   "c" 'ranger-up-directory		; c
   "j" 'ranger-toggle-mark		; t
   )

  :config
  (setq ranger-cleanup-eagerly t))

(use-package recentf
  :commands (recentf-mode
             counsel-recentf)
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
  :config
  (setq recentf-max-saved-items 50))

(use-package restart-emacs :ensure t
  :commands restart-emacs)

;; ---------- S --------------------------------------------------
(use-package scratch :ensure t
  :bind* (("s-n" . scratch)))

(use-package scss-mode :ensure t
  :mode ("\\.scss\\'" . scss-mode))

(use-package sh-script :defer t
  :mode
  ( ("\\.zsh\\'" . sh-mode)
    ("zlogin\\'" . sh-mode)
    ("zlogout\\'" . sh-mode  )
    ("zpreztorc\\'" . sh-mode )
    ("zprofile\\'" . sh-mode )
    ("zshenv\\'" . sh-mode )
    ("zshrc\\'" . sh-mode))
  :config

  (use-package company-shell
    :quelpa (company-shell :fetcher github :repo "Alexander-Miller/company-shell")
    :config
    (add-to-list 'company-backends 'company-shell)))

(use-package smartparens
  :ensure t
  :diminish (smartparens-mode . "")
  :defines hydra-sp/body
  :commands (smartparens-global-mode
             hydra-sp/body)
  :bind* (("C-M-f" . sp-forward-sexp)
          ("C-M-b" . sp-backward-sexp)
          ("C-M-d" . sp-down-sexp)
          ("C-M-a" . sp-backward-down-sexp)
          ("C-S-d" . sp-beginning-of-sexp)
          ("C-S-a" . sp-end-of-sexp)
          ("C-M-e" . sp-up-sexp)
          ("C-M-u" . sp-backward-up-sexp)
          ("C-M-t" . sp-transpose-sexp)
          ("C-S-r" . sp-forward-slurp-sexp)
          ("C-S-c" . sp-forward-barf-sexp)
          ("C-S-s" . sp-backward-slurp-sexp)
          ("C-S-t" . sp-backward-barf-sexp)
          ("C-{"   . sp-backward-barf-sexp)
          ("C-}"   . sp-slurp-hybrid-sexp)
          ("C-S-b" . sp-backward-symbol)
          ("C-S-f" . sp-forward-symbol))
  :init
  (add-hook 'after-init-hook (lambda () (smartparens-global-mode)))
  (add-hook 'prog-mode-hook (lambda () (smartparens-strict-mode)))

  :config
  ;; Only use smartparens in web-mode
  (sp-local-pair 'markdown-mode "_" "_")
  (sp-local-pair 'markdown-mode "**" "**")
  (sp-local-pair 'markdown-mode "`" "`")
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
  (sp-pair "'" nil :actions :rem)

  ;; from hydra wiki
  (defhydra hydra-sp (:hint nil)
    "
       ^Navigate^   ^^              ^Slurp - Barf^    ^Splice^          ^Commands^
       ^--------^   ^^              ^------------^    ^------^          ^--------^
    _b_: bwd      _f_: fwd       _c_: slurp bwd      _ll_: splice    _x_: del char
    _T_: bwd ↓    _S_: bwd ↑     _r_: slurp fwd      _lf_: fwd       _é_: del word
    _t_: ↓        _s_: ↑       _C-c_: barf bwd       _lb_: bwd       _(_: del sexp
    _p_: prev     _n_: next    _C-r_: barf fwd       _la_: around    _w_: unwrap
  _M-p_: end    _M-n_: begin   ^   ^                 ^  ^
       ^------^     ^^             ^------------^
       ^Symbol^     ^^             ^join - split^
       ^------^     ^^             ^------------^
  _C-b_: bwd    _C-b_: fwd       _j_: join
      ^^            ^^           _v_: split
"
    ("b" sp-backward-sexp )
    ("f" sp-forward-sexp )
    ("T" sp-backward-down-sexp )
    ("S" sp-backward-up-sexp )
    ("t" sp-down-sexp )                 ; root - towards the root
    ("s" sp-up-sexp )
    ("n" sp-next-sexp )
    ("p" sp-previous-sexp )
    ("M-n" sp-beginning-of-sexp)
    ("M-p" sp-end-of-sexp )
    ("x" sp-delete-char )
    ("é" sp-kill-word )
    ("(" sp-kill-sexp )
    ("w" sp-unwrap-sexp ) ;; Strip!
    ("r" sp-forward-slurp-sexp )
    ("C-r" sp-forward-barf-sexp )
    ("c" sp-backward-slurp-sexp )
    ("C-c" sp-backward-barf-sexp )
    ("ll" sp-splice-sexp )
    ("lf" sp-splice-sexp-killing-forward )
    ("lb" sp-splice-sexp-killing-backward )
    ("la" sp-splice-sexp-killing-around )
    ("C-f" sp-forward-symbol )
    ("C-b" sp-backward-symbol )
    ("j" sp-join-sexp ) ;;Good
    ("v" sp-split-sexp )
    ("u" undo "undo")
    ("q" nil "quit" :color blue))

  (defun sam--create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    ;; from https://github.com/Fuco1/smartparens/issues/80
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(use-package smex
  :quelpa (smex :fetcher github :repo "abo-abo/smex"))

(use-package smooth-scrolling :ensure t
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 5))

(use-package subword :defer t
  :init
  (add-hook 'prog-mode-hook (lambda () (subword-mode 1)))
  :diminish "")

(use-package swiper :ensure t
  :bind* ("s-s" . swiper))

;; ---------- T --------------------------------------------------
(use-package tex
  :ensure auctex
  :commands init-auctex
  :defines init-auctex
  :init
  (add-hook 'LaTeX-mode-hook 'latex-auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  :config
  (load-file "~/dotfile/emacs/latex-config.el"))

(use-package textpy
  :load-path "~/.emacs.d/private/textpy"
  :diminish "☉"
  :init
  (add-hook 'text-mode-hook (lambda () (textpy-minor-mode 1)))
  :commands
  (textpy-minor-mode)
  :config
  (general-define-key
   :keymaps 'text-mode-map
    "A" 'textpy-avy-sentence
    "a" 'textpy-avy-jump
    "b" 'textpy-backward-sentence
    "c" 'textpy-correct
    "d" 'textpy-delete-buffer
    "f" 'textpy-forward-sentence
    "q" 'textpy-fill-paragraph
    "s" 'textpy-save
    "v" 'textpy-git
    "/" 'textpy-search))

(use-package tile :ensure t
  :defines (hydra-tile/body)
  :bind* (("C-c t" . hydra-tile/body)
          ("s-t" . tile))
  :config


  (tile :strategy tile-master-right-3)
  (tile :strategy tile-master-left-3)
  (tile :strategy tile-master-top-3)
  (tile :strategy tile-wide-3)
  (tile :strategy tile-wide)
  (tile :strategy tile-one)

  (defhydra hydra-tile (:hint nil :color red :columns 4)
    "tile "
    ("a" (tile :strategy tile-tall-3) "tall 3")
    ("u" (tile :strategy (tile-split-n-tall 4)) "tall 4")
    ("i" (tile :strategy (tile-split-n-wide 2)) "wide 2")
    ("e" (tile :strategy (tile-split-n-wide 3)) "wide 3")
    ("c" (tile :strategy tile-master-left-3) "left 3")
    ("t" (tile :strategy tile-master-bottom-3) "bottom 3")
    ("s" (tile :strategy tile-master-top-3) "top 3")
    ("r" (tile :strategy tile-master-right-3) "right 3")

    ("m" tile-select "chose")
    ("w" (tile :strategy tile-one) "one")
    ("n" tile "tile")
    ("U" winner-undo "undo")
    ("é" hydra-window/body "windows" :color blue)

    ("q" nil :color blue "quit"))

  (setq tile-cycler (tile-strategies :strategies
                      (list tile-strategy-tall
                            tile-strategy-master
                            tile-strategy-wide
                            tile-one))))

(use-package tiny :ensure t
  :bind* (("C-;" . tiny-expand)))

;; ---------- U --------------------------------------------------
(use-package undo-tree :ensure t
  :diminish undo-tree-mode
  :bind* (("C-x u" . undo-tree-visualize)
          ("C-z" . undo-tree-undo)
          ("C-S-z" . undo-tree-redo)))

;; ---------- V --------------------------------------------------
(use-package visual-regexp-steroids :ensure t
  :bind* (("s-%" . vr/replace)
          ("M-s-%" . vr/query-replace))
  :config
  (setq vr/auto-show-help nil))

;; ---------- W --------------------------------------------------
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
   ("\\.djhtml\\'"     . web-mode))
  :config
  (use-package company-web :ensure t
    :config
    (add-to-list 'company-backends 'company-web-html))
  (add-hook 'web-mode-hook (lambda ()
                             (setq-local tab-width 2)
                             (setq-local outline-regexp "<!--*"))))

(use-package wgrep :ensure t :defer t)

(use-package which-key :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.5
        which-key-side-window-max-width 0.33
        which-key-idle-delay 0.5
        which-key-min-display-lines 7))

(use-package whitespace
  :diminish ""
  :commands whitespace-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  :config
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail)))

(use-package window-numbering :ensure t
  :commands
  (window-numbering-mode
   select-window-0
   select-window-1
   select-window-2
   select-window-3
   select-window-4
   select-window-5
   select-window-6
   select-window-7
   select-window-8
   select-window-9)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by the powerline.")
  (window-numbering-install-mode-line))

(use-package wrap-region :ensure t
  :diminish ""
  :config
  (wrap-region-mode 1)
  (wrap-region-add-wrappers
   '(("$" "$")
     ("{-" "-}" "#")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode ruby-mode))
     ("*" "*" nil (markdown-mode))
     ("_" "_" nil (markdown-mode)))))

;; ---------- X --------------------------------------------------
(use-package xterm-color
  :quelpa (xterm-color :fetcher github :repo "atomontage/xterm-color")
  :config
  ;; comint install
  (progn
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))))

;; ---------- Y --------------------------------------------------
(use-package yaml-mode :ensure t :defer t)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-global-mode)
  :defer 10
  :init
  (with-eval-after-load 'yasnippet
    (progn
      (setq yas-snippet-dirs
            (append yas-snippet-dirs '("~/dotfile/emacs/snippets")))))
  :config
  (yas-global-mode)
  (setq yas-indent-line 'none))

;; ---------- Z --------------------------------------------------
(use-package zerodark-theme :ensure t
  :defer 1
  :config
  (zerodark-setup-modeline-format))

(use-package zoom-frm :ensure t
  :commands
  (zoom-frm-in
   zoom-frm-out
   zoom-frm-unzoom
   zoom-in
   zoom-out)
  :config
  (setq zoom-frame/buffer 'buffer))

;;; -------------------------------------------------------------------

;;; personal functions
(load-file "~/dotfile/emacs/functions.el")
;;; org
(load-file "~/dotfile/emacs/org.el")
;; ---------- keybindings -------------------------------------------------
(load-file "~/dotfile/emacs/keybindings.el")

;;; custom
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
(put 'erase-buffer 'disabled nil)
