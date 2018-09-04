;; -*- lexical-binding: t -*-

;;; Licence

;; Copyright (C) 2017 Samuel Barreto <samuel.barreto8@gmail.com>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;

;;; Functions & Macros

(defmacro add-hook! (hook &rest body)
  "Nicer add-hooking that prevents writing lambdas explicitely.

Add a lamdba containing BODY to hook HOOK."
  (declare (indent 1))
  `(add-hook ,hook
             (lambda () ,@body)))


;;; Package.el

(defun sam--increase-gc-threshold! ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun sam--normal-gc-threshold! ()
  (setq gc-cons-threshold 800000))

(add-hook! 'minibuffer-setup-hook
  (sam--increase-gc-threshold!))

(add-hook! 'minibuffer-exit-hook
  (sam--normal-gc-threshold!))

(sam--increase-gc-threshold!)
(add-hook! 'after-init-hook
  ;; restore after startup
  (sam--normal-gc-threshold!))

(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
   (normal-top-level-add-subdirs-to-load-path))
(package-initialize t)
;; (require 'use-package)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package package
  :config
  (setq package-check-signature nil)
  (setq package-enable-at-startup nil)
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("elpy" . "https://jorgenschaefer.github.io/packages/"))))

(use-package general :ensure t)
(use-package diminish)
(use-package bind-key)

(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;; Sane default

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

(setq
 use-package-verbose nil      ; use-package décrit les appels qu'il fait
 delete-old-versions -1	; supprime les vieilles versions des fichiers sauvegardés
 version-control t      ; enable le version control
 vc-make-backup-files t	; backups file even when under vc
 vc-follow-symlinks t	; vc suit les liens  symboliques
 auto-save-file-name-transforms
 '((".*" "~/.emacs.d/auto-save-list/" t)) ; transforme les noms des fichiers sauvegardés
 inhibit-startup-screen t                 ; supprime l'écran d'accueil
 ring-bell-function 'ignore           ; supprime cette putain de cloche.
 sentence-end-double-space nil ; sentences does not end with double space.
 default-fill-column 72
 initial-scratch-message ""
 save-interprogram-paste-before-kill t
 help-window-select t                   ; focus help window when opened
 tab-width 4                            ; tab are 4 spaces large
 auto-window-vscroll nil
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
(line-number-mode -1) ; display line number in mode line
(column-number-mode -1) ; display colum number in mode line
(save-place-mode)    ; save cursor position between sessions
(delete-selection-mode 1)               ; replace highlighted text with type
(setq initial-major-mode 'fundamental-mode)
;; supprime les caractères en trop en sauvegardant.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; rend les scripts executable par défault si c'est un script.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defvar my-font-for-light "DejaVu Sans Mono 11"
  "Font for coding situations.")

;;; apparences

(tooltip-mode -1)      ; don't know what that is
(tool-bar-mode -1)     ; sans barre d'outil
(menu-bar-mode -1)     ; barre de menu
(scroll-bar-mode -1)   ; enlève la barre de défilement
(blink-cursor-mode -1) ; pas de clignotement

(when window-system
  (set-frame-size (selected-frame) 85 61)
  (add-to-list 'default-frame-alist '(height . 46))
  (add-to-list 'default-frame-alist '(width . 85))
  (fringe-mode '(4 . 0))                ; reduce fringe size to 4 px
  (setq-default line-spacing 6)         ; increase between-line space.

  ;; change default font for current frame
  (add-to-list 'default-frame-alist `(font . ,my-font-for-light))
  (set-face-attribute 'default nil :font my-font-for-light))

(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

(defun sam--set-initial-frame ()
  "Set the default dimension and position of a new frame."
  (let* ((a-width (* (display-pixel-width) 0.50))
         (a-height (* (display-pixel-height) 0.60))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))

    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame)
                    (truncate a-width)
                    (truncate a-height)
                    t )))

(sam--set-initial-frame)

(setq
 display-buffer-alist
 `(("\\*Buffer List\\*" display-buffer-in-side-window
    (side . top)
    (slot . -1)
    (window-height . 20)
    (preserve-size . (nil . t)) ,parameters)
   ("\\*Tags List\\*" display-buffer-in-side-window
    (side . right)
    (slot . 1)
    (window-width . fit-window-to-buffer)
    (preserve-size . (t . nil)) ,parameters)
   ("\\*\\(?:help\\|grep\\|Completions\\)\\*"
    display-buffer-in-side-window
    (side . bottom)
    (slot . -1)
    (preserve-size . (nil . t))
    ,parameters)
   ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
    (side . bottom)
    (slot . 1)
    (preserve-size . (nil . t))
    ,parameters)
   ("\\*Org Select\\*" display-buffer-in-side-window
    (side . top)
    (slot . -1)
    (window-width . fit-window-to-buffer)
    (preserve-size . (t . nil))
    ,parameters)))

;;; keybindings

(when (eq system-type 'darwin)           ; mac specific bindings
  (setq mac-right-command-modifier 'meta ; cmd de droite = meta
        mac-command-modifier 'control    ; cmd de gauche = control
        mac-option-modifier 'super       ; option de gauche = super
        mac-right-option-modifier nil    ; option de droite = carac spéciaux
        mac-control-modifier 'hyper      ; control de gauche = hyper (so does capslock)
        ns-function-modifier 'hyper      ; fn key = hyper
        ns-right-alternate-modifier nil) ; cette touche n'existe pas.
  (setq mac-pass-command-to-system nil)  ; disable system call to commands like
                                         ; C-h (hide frame on macOS by default
  (setq mac-pass-control-to-system nil)  ; idem
  (setq-default locate-command "mdfind")



  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil nil nil file)))

;;; Packages

;;;; A

(use-package abbrev :defer t
  :diminish ""
  :init
  (add-hook! 'text-mode-hook
    (abbrev-mode))
  ;; tell emacs where to read abbrev definitions from...
  (setq abbrev-file-name "~/dotfile/emacs/.abbrev_defs")
  (setq save-abbrevs 'silently)
  (setq only-global-abbrevs t)
  (setq-default abbrev-mode t))

(use-package ace-window :ensure t
  :commands (ace-window)
  :config
  (setq aw-keys '(?t ?s ?r ?n ?m ?a ?u ?i ?e))
  (setq aw-background t)
  (setq aw-ignore-current t))

(use-package ansi-color
  :config
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (ignore-errors
    (defun my-colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

(use-package anzu
  :defer 0.2
  :ensure t
  :diminish ""
  :bind*
  (("C-0" . anzu-query-replace-at-cursor)
   ("C-9" . anzu-replace-at-cursor-thing))
  :config
  (global-anzu-mode t)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package auto-fill-mode
  :diminish auto-fill-function
  :commands turn-on-auto-fill
  :init
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (diminish 'auto-fill-function ""))

(use-package autoinsert
  :init
  (auto-insert-mode)
  (setq auto-insert-directory "~/dotfile/emacs/autoinsert"))

;; mainly to make autorevert disappear from the modeline
(use-package autorevert
  :defer t
  :diminish auto-revert-mode)

(use-package avy :ensure t
  :after key-seq
  :commands (avy-goto-word-or-subword-1
             avy-goto-word-1
             avy-goto-char-in-line
             avy-goto-line
             avy-goto-char)
  :config
  (key-seq-define-global "qé" #'avy-resume)
  (key-seq-define-global "qp" #'avy-goto-word-1)
  (key-seq-define-global "qe" #'avy-goto-char-in-line)
  (key-seq-define-global "ql" #'avy-goto-line)
  (setq avy-keys '(?a ?t ?u ?s ?i ?r ?e ?n ?p ?d ?é ?l))
  (setq avy-all-windows nil)
  (setq avy-styles-alist
        '((avy-goto-char-in-line . post)
          (avy-goto-word-or-subword-1 . post)
          (avy-goto-word-1 . pre))))

;;;; B

(use-package bash-completion
  :ensure t
  :hook (shell-dynamic-complete-functions . bash-completion-dynamic-complete)
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook"))

;; (use-package bioseq-mode
;;   :load-path "~/.emacs.d/private/bioseq-mode"
;;   :commands (bioseq-mode))

(use-package biblio
  :ensure t
  :after ivy-bibtex)

(use-package blank-mode
  :ensure t
  :commands blank-mode)

(use-package blink-cursor
  :commands (blink-cursor-mode))

;;;; C

(use-package cc-mode
  :ensure t
  :mode (("\\.c\\'" . c-mode)
         ("\\.h\\'" . c-mode)
         ("\\.cpp\\'" . cpp-mode)
         ("\\.hpp\\'" . cpp-mode)))

(use-package cquery
  :ensure t
  :functions (sam|cquery-enable)
  :hook (c-mode-hook . sam|cquery-enable)
  :config
  (defun sam|cquery-enable ()
    (interactive)
    (ignore-errors (lsp-cquery-enable)))

  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
  (setq cquery-extra-init-params '(:completion (:detailedLabel t)))
  (setq cquery-sem-highlight-method 'font-lock)
  (setq cquery-executable "/usr/local/bin/cquery"))

(use-package conf-mode
  :mode (("DESCRIPTION" . conf-mode)
         ("\\.log\\'" . conf-mode)
         ("\\.toml\\'" . conf-toml-mode)))

(use-package command-log-mode
  :ensure t
  :commands (command-log-mode))

(use-package company
  :ensure t
  :diminish ""
  :bind* (("C-c C-i" . company-mode))
  :commands (company-mode)
  :init
  ;; -- [2018-05-29 09:58] tentative de non utilisation de company.
  ;; (add-hook 'after-init-hook #'global-company-mode)

  (defun set-local-company-backends! (backend)
    "Add a backend to company locally."
    (with-eval-after-load 'company
      (setq-local company-backends
                  (pcase backend
                    ((pred listp) (append backend company-backends))
                    ((pred symbolp) (cons backend company-backends))
                    (_ company-backends)))))

  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.2
        company-selection-wrap-around t
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers t)

  :config
  ;; (setq company-backends
  ;;       (delete 'company-semantic company-backends))

  (bind-keys :map company-active-map
    ("C-d" . company-show-doc-buffer)
    ("C-l" . company-show-location)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-t" . company-select-next)
    ("C-s" . company-select-previous)
    ("TAB" . company-complete))

  ;; from https://github.com/syl20bnr/spacemacs/blob/master/layers/auto-completion/packages.el
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package company-bibtex
  :ensure t
  :after company
  :config
  (setq company-bibtex-bibliography '("~/Dropbox/bibliography/references.bib"))
  (add-hook! 'latex-mode-hook
    (set-local-company-backends! 'company-bibtex)))

(use-package company-childframe
  :disabled t
  :load-path "~/.emacs.d/private/company-childframe"
  :after posframe
  :config
  (company-childframe-mode t)
  (require 'desktop)
  (push '(company-childframe-mode . nil) desktop-minor-mode-table))

(use-package company-statistics
  :ensure t
  :after company
  :config
  (company-statistics-mode))

(use-package counsel :ensure
  :commands (counsel-load-theme
             counsel-bookmark)
  :bind* (("C-c /" . counsel-rg)
          ("C-c i" . counsel-imenu)
          ("C-x C-f" . counsel-find-file)
          ("C-x C-b" . ivy-switch-buffer)
          ("s-<backspace>" . ivy-switch-buffer)
          ("M-x" . counsel-M-x))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"))

  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-at-point t))

(use-package counsel-gtags
  :ensure t
  :after ggtags
  :bind (:map c-mode-map
         ("C-c g g" . counsel-gtags-dwim)
         ("C-c g s" . counsel-gtags-find-symbol)
         ("C-c g d" . counsel-gtags-find-definition)
         ("C-c g r" . counsel-gtags-find-reference)
         ("C-c g f" . counsel-gtags-find-file)
         ("C-c g c" . counsel-gtags-create-tags)
         ("C-c g u" . counsel-gtags-update-tags)))

(use-package css-mode :ensure t
  :mode (("\\.css\\'" . css-mode)))

;;;; D

(use-package deadgrep
  :load-path "~/.emacs.d/private/deadgrep"
  :bind* (("C-c /" . deadgrep)))

(use-package define-word
  :ensure t
  :commands (define-word
              define-word-at-point))

(use-package dired
  :bind* (("C-x d" . dired-other-window)
          ("C-x C-d" . dired-side))
  :commands (dired
             dired-side)
  :functions (dired-side)
  :config
  (defun dired-side ()
    "Display `default-directory' in side window on left, hiding details."
    (interactive)
    (let ((buffer (dired-noselect default-directory)))
      (with-current-buffer buffer (dired-hide-details-mode t))
      (display-buffer-in-side-window
       buffer  `((side          . left)
                 (slot          . 0)
                 (window-width  . fit-window-to-buffer)
                 (preserve-size . (t . nil)) ,parameters))))

  (use-package dired-x
    :bind* (("C-x C-'" . dired-jump))
    :commands (dired-omit-mode)
    :init
    (add-hook! 'dired-load-hook
      (load "dired-x"))
    (add-hook! 'dired-mode-hook
      (dired-omit-mode))
    :config
    (setq dired-omit-verbose nil)
    (setq dired-omit-extensions
          (cl-list* ".aux" ".fls" ".log" ".out" ".toc" ".fdb_latexmk" ".bbl" ".blg" ".bcf" ".run.xml" ".x" ".d" dired-omit-extensions))
    (setq dired-omit-files (concat dired-omit-files
                                   "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))

  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))

  (setq ls-lisp-use-insert-directory-program t)
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t) ; guess copy target based on other dired window

  (defun dired-view-other-window ()
    "View the current file in another window (possibly newly created)."
    (interactive)
    (if (not (window-parent))
        (split-window))
    (let ((file (dired-get-file-for-visit))
          (dbuffer (current-buffer)))
      (other-window 1)
      (unless (equal dbuffer (current-buffer))
        (if (or view-mode (equal major-mode 'dired-mode))
            (kill-buffer)))
      (let ((filebuffer (get-file-buffer file)))
        (if filebuffer
            (switch-to-buffer filebuffer)
          (view-file file))
        (other-window -1))))

  (defun dired-mkdir-date (dir-name)
    "Make a directory with current date style"
    (interactive "sDirectory content: ")
    (mkdir (format "%s-%s" (format-time-string "%Y-%m-%d" (current-time)) dir-name))
    (revert-buffer))

  (defun dired-mkdir-date-rstyle (dir-name)
    (interactive "sDirectory content: ")
    (mkdir (format "%s.%s" dir-name (format-time-string "%Y%m%d" (current-time))))
    (revert-buffer))

  (bind-keys :map dired-mode-map
    ("SPC" . dired-view-other-window)
    ("."   . hydra-dired-main/body)
    ("t"   . dired-next-line)
    ("s"   . dired-previous-line)
    ("r"   . dired-find-file)
    ("c"   . dired-up-directory)
    ("'"   . eshell-here)
    ("8"   . dired-mkdir-date)
    ("9"   . dired-mkdir-date-rstyle)
    ("O"   . sam|open-in-external-app)
    ("C-'" . shell)
    ("q"   . (lambda () (interactive) (quit-window 4)))))

(use-package doom-modeline
  :load-path "~/.emacs.d/private/doom-modeline/"
  :init
  (use-package shrink-path
    :ensure t)

  (use-package eldoc-eval
    :ensure t
    :commands (eldoc-in-minibuffer-mode))

  :config
  (doom-modeline-init)
  (setq doom-modeline-height 20))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;;;; E

(use-package ediff
  :commands (ediff ediff3)
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package eldoc
  :ensure t
  :commands (turn-on-eldoc-mode)
  :diminish ""
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (turn-on-eldoc-mode))
  (add-hook! 'lisp-interaction-mode-hook
    (turn-on-eldoc-mode)))

(use-package elfeed
  :ensure t
  :bind* (("<f6>" . elfeed)
          ("C-c E" . elfeed))
  :config
  ;; increase title width to accomodate papers
  (setq elfeed-search-title-max-width 120)

  (setq elfeed-db-directory "~/dotfile/emacs/elfeed/")

  (use-package elfeed-org
    :ensure t
    :init
    (setq rmh-elfeed-org-files (list "~/dotfile/emacs/elfeed.org"))
    :functions (elfeed-org-minor-mode)
    :config
    (elfeed-org)

    (define-minor-mode elfeed-org-minor-mode
      "Minor mode to interact with elfeed from the elfeed org mode
  buffer."
      :lighter " eorg"
      :keymap (let ((map (make-sparse-keymap)))
                (define-key map (kbd "RET") #'elfeed-org-filter-entry)
                map))

    (defun elfeed-org-get-tags-not-properties (s)
      (if (get-text-property 0 'inherited s)
          (substring-no-properties s)
        s))

    (defun elfeed-org-filter-entry ()
      (interactive)
      (let* ((buf (elfeed-search-buffer))
             (p (point))
             (tag-at-point (org-get-tags-at p))
             (tags (mapcar #'elfeed-org-get-tags-not-properties tag-at-point))
             (clean-tags (delete rmh-elfeed-org-tree-id tags))
             (tag-filter (mapconcat (lambda (x) (concat " +" x)) clean-tags ""))
             (new-filter (concat (default-value 'elfeed-search-filter)
                                 tag-filter)))
        (elfeed-search-set-filter new-filter)
        (unless (get-buffer-window buf 'visible)
          (switch-to-buffer-other-window buf)
          (switch-to-prev-buffer)))))

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (bind-keys :map elfeed-search-mode-map
    ("N" . elfeed-next-tag)
    ("f" . hydra-elfeed-navigate/body)
    ("U" . elfeed-search-fetch)
    ("." . hydra-elfeed/body)
    ("R" . elfeed-mark-all-as-read))

  (defvar counsel-elfeed-tags
    '(("papers" . "+papers")
      ("biology" . "+bio")
      ("bioinfo" . "+bioinfo")
      ("computer science" . "+compsci")
      ("package update" . "+pkg")
      ("rstat" . "+rstat")
      ("emacs" . "+emacs")
      ("news" . "+news")
      ("communisme libertaire" . "+anar")
      ("communisme marxiste" . "+communisme"))
    "This is a list of tag for elfeed filtering.")

  (defun elfeed-update-filter (x)
    (setf elfeed-search-filter
          (concat "@6-months-ago +unread " x))
    (elfeed-search-update :force))

  (defun elfeed-next-tag ()
    (interactive)
    (let* ((elfeed-tag-list
            (with-current-buffer
                (find-file-noselect (expand-file-name (car rmh-elfeed-org-files)))
              (org-mode)
              (mapcar
               (lambda (x) (concat "+" x))
               (cdr (reverse (mapcar #'car (org-get-buffer-tags)))))))
           (current-tag
            (car (last (split-string elfeed-search-filter))))
           (new-tag
            (cadr (member current-tag elfeed-tag-list))))
      (if new-tag
          (elfeed-update-filter new-tag)
        (elfeed-update-filter (car elfeed-tag-list)))))

  (defun counsel-elfeed--update-tag (x)
    "Update the elfeed filter with the last 10 char of ivy selection."
    (elfeed-search-set-filter
     (concat "@6-months-ago +unread " x)))

  (defun counsel-elfeed-tag ()
    "Shows a list of elfeed tags I like to browse."
    (interactive)
    (ivy-read "%d Elfeed tag: "
              (with-current-buffer
                  (find-file-noselect (expand-file-name (car rmh-elfeed-org-files)))
                (org-mode)
                (mapcar
                 (lambda (x) (concat "+" x))
                 (cdr (reverse (mapcar #'car (org-get-buffer-tags))))))
              :require-match t
              :action #'counsel-elfeed--update-tag
              :update-fn (lambda ()
                           (counsel-elfeed--update-tag (ivy-state-current ivy-last)))
              :caller 'counsel-elfeed-tag
              :sort t))

  (setq-default elfeed-search-filter "@6-months-ago +unread"))



(use-package emacs-lisp-mode
  :mode
  (("*scratch*" . emacs-lisp-mode))
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
    (set-local-company-backends! 'company-elisp))

  (general-define-key
   :keymaps 'emacs-lisp-mode-map
   :prefix "ê"
   "u" 'use-package-jump))

(use-package esup
  :ensure t
  :commands esup)

(use-package etymologie
  :load-path "~/.emacs.d/private/etymologie"
  :commands (etymologie))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :defer 1
  :commands (exec-path-from-shell-initialize
             exec-path-from-shell-copy-env)
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :ensure t
  :defines hydra-expand-region/body
  :commands (er/expand-region
             er/mark-word
             er/mark-symbol
             er/mark-symbol-with-prefix
             er/mark-next-accessor
             er/mark-method-call
             er/mark-inside-quotes
             er/mark-outside-quotes
             er/mark-inside-pairs
             er/mark-outside-pairs
             er/mark-comment
             er/mark-sentence
             er/mark-paragraph
             er/mark-url
             er/mark-email
             er/mark-defun)
  :bind (("C-=" . er/expand-region)
         ("s-=" . hydra-expand-region/er/expand-region)
         ("C-°" . er/contract-region)
         ("s-°" . hydra-expand-region/er/contract-region))
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

;;;; F

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

(use-package flx :ensure t)

(use-package flycheck
  :disabled t
  :ensure t
  :commands (flycheck-mode)
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

(use-package flymake
  :commands (flymake-mode
             hydra-errors/body)
  :functions (hydra-errors/body)
  :config
  (defhydra hydra-errors (:body-pre (flymake-mode)
                          :post (flymake-mode -1)
                          :hint nil)
    "
Linting : [_n_]ext / [_p_]rev
Errors  : [_t_]: next / [_s_]: prev
Totos   : _C-n_: next / _C-p_: prev / _C-s_: search"
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("t" next-error)
    ("s" previous-error)
    ("C-n" hl-todo-next)
    ("C-p" hl-todo-previous)
    ("C-s" hl-todo-occur)))

(use-package flyspell
  :bind* (("C-c M-f" . flyspell-mode)))

;;;; G

(use-package geiser
  :mode (("\\.scm\\'" . scheme-mode))
  :hook (scheme-mode . geiser-mode)
  :commands (geiser-mode
             connect-to-guile
             geiser-connect
             geiser)
  :init
  (setq geiser-active-implementations '(guile chez))
  (setq geiser-guile-load-path '("/usr/local/Cellar/guile/2.2.2/share/guile/"))
  (setq geiser-guile-load-init-file-p t)
  (setq geiser-guile-manual-lookup-nodes '("Guile" "guile-2.2"))
  (setq geiser-guile-binary "/usr/local/bin/guile")
  :config
  (require 'geiser-install))

;; gengetopt mode
(use-package ggo-mode
  :ensure t
  :mode ("\\.ggo\\'" . ggo-mode))

(use-package ggtags :ensure t
  :commands (ggtags-mode)
  :bind (:map ggtags-mode-map
         ("M-?" . ggtags-find-reference)))

(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
              ("<RET>" . goto-address-at-point)
              ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

(use-package goto-chg :ensure t
  :commands (goto-last-change
             goto-last-change-reverse))

(use-package gpl                        ; package to add gpl to file
  :load-path "~/dotfile/emacs/emacs.d/private/gpl"
  :commands (gpl-python gpl-cc-copyright-line gpl-pythonL
             gpl-library-notice gpl-cL gpl-cc gpl-shL gpl-el
             gpl-shS gpl-texinfo gpl-m4 gpl-insert-notice gpl-sh
             gpl-ccL gpl-python-copyright-line gpl-test
             gpl-c-copyright-line gpl-c))

(use-package grab-mac-link
  :ensure t
  :commands grab-mac-link)


;;;; H

(use-package helm-make
  :bind* (("C-c C" . helm-make)
          ("C-c p c" . helm-make-projectile))
  :config
  (setq helm-make-completion-method 'ivy))

(use-package helpful
  :ensure t
  :init
  (use-package shut-up :ensure t :defer t)
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

(use-package highlight-indent-guides
  :ensure t
  :commands (highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package highlight-indentation
  :ensure t
  :defer t)

(use-package hideshow
  :diminish (hs-minor-mode . "")
  :hook ((prog-mode . hs-minor-mode)
         (latex-mode . hs-minor-mode))
  :bind* (("s-h" . hs-toggle-hiding)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :commands (hl-todo-previous
             hl-todo-next
             hl-todo-occur))

(use-package htmlize
  :ensure t
  :defer t)

(use-package hungry-delete
  :ensure t
  :diminish ""
  :config
  (global-hungry-delete-mode))

(use-package hydra :ensure t
  :config
  (setq hydra-is-helpful t))

(use-package hy-mode
  :ensure t
  :mode ("\\.hy\\'" . hy-mode))

;;;; I

(use-package ibuffer
  :ensure t
  :bind* (("C-x b" . ibuffer))
  :commands ibuffer
  :init
  (add-hook! 'ibuffer-hook
    (ibuffer-switch-to-saved-filter-groups "Default"))
  :config
  (use-package ibuffer-vc :ensure t
    :config
    (setq ibuffer-vc-set-filter-groups-by-vc-root t))

  (define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)
  (general-define-key
   :keymaps 'ibuffer-mode-map
   "." 'hydra-ibuffer-main/body
   "t" 'next-line
   "s" 'previous-line
   "r" 'ibuffer-visit-buffer
   "c" 'ibuffer-backward-filter-group
   "p" 'ibuffer-backward-filter-group
   "n" 'ibuffer-forward-filter-group)

  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))

  (setq-default ibuffer-saved-filter-groups
                `(("Default"
                   ("RStat" (or (mode . ess-mode)
                                (mode . ess-help-mode)
                                (mode . inferior-ess-mode)))
                   ("C / C++" (mode . c-mode))
                   ("Org" (mode . org-mode))
                   ("Markdown" (mode . markdown-mode))
                   ("Bash" (or (mode . shell-script-mode)))
                   ("Make" (mode . makefile-mode))
                   ("Dired" (mode . dired-mode))
                   ("PDF" (mode . pdf-view-mode))
                   ("Mail" (or (mode . message-mode)
                               (mode . bbdb-mode)
                               (mode . mail-mode)
                               (mode . mu4e-compose-mode)))
                   ("Elisp" (mode . emacs-lisp-mode))
                   ("Scheme" (mode . scheme-mode))
                   ("shell" (or (mode . eshell-mode)
                                (mode . shell-mode)))
                   ("Magit" (or (mode . magit-mode)))
                   ("Temp" (name . "\*.*\*")))))
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-display-summary t))

(use-package iedit :ensure t
  :defines hydra-iedit/body
  :bind* (:map global-map
          ("C-*" . iedit-mode)
          :map iedit-mode-keymap
          ("M-n" . iedit-next-occurence)
          ("M-p" . iedit-prev-occurence))
  :config
  (defhydra hydra-iedit (:color pink :columns 1)
    "IEDIT"
    ("C-*" iedit-mode "toggle")
    ("C-p" iedit-prev-occurrence "prev")
    ("C-n" iedit-next-occurrence "next")
    ("C-g" iedit-quit "toggle" :color blue)))

(use-package "isearch"
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
  :ensure t
  :diminish ""
  :after (counsel swiper)
  :custom
  (ivy-display-style 'fancy)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1)
  (setq ivy-display-function nil))

(use-package ivy-bibtex
  :ensure t
  :bind ("C-x M-b" . ivy-bibtex)
  :config

  (setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq bibtex-completion-library-path '("~/zotero_bib/"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-path "~/dotfile/bibliographie/notes.org")
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))))

;;;; L

(use-package latex-unicode-math-mode :ensure t
  :commands (latex-unicode-math-mode
             latex-unicode-mode)
  :config
  (use-package company-math :ensure t
    :init
    (add-hook! 'ess-julia-mode-hook
      (set-local-company-backends! 'company-math-symbols-latex))))

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
   "E" 'lesspy-avy-eval
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
   "#" 'lesspy-comment
   "'" 'lesspy-roxigen
   "C" 'lesspy-cleanup-pipeline
   "C-<return>" 'lesspy-send-line
   "M-RET" 'lesspy-send-function-or-paragraph
   "C-d" 'lesspy-kill-forward
   "C-(" 'lesspy-paren-wrap-next
   "DEL" 'lesspy-kill-backward)
  (general-define-key
   :keymaps 'inferior-ess-mode-map
   "C-(" 'lesspy-paren-wrap-next))

(use-package lispy :ensure t
  :diminish (lispy-mode . " λ")
  :commands lispy-mode
  :init
  (add-hook! 'emacs-lisp-mode-hook
    (lispy-mode 1))
  (add-hook! 'lisp-mode-hook
    (lispy-mode 1))

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

(use-package lua-mode

;;;; M

(use-package magit
  :ensure t
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

  (general-define-key
   :keymaps 'magit-mode-map
   "'" #'eshell-here)

  (use-package magit-popup :ensure t)
  (use-package git-commit :ensure t :defer t)

  (use-package magit-gitflow :ensure t
    :commands turn-on-magit-gitflow
    :bind (:map magit-mode-map
           ("%" . magit-gitflow-popup))
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-buffer-name-format "%x%M%v: %t%x"))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode))

(use-package make-it-so
  :ensure t
  :config
  (setq mis-recipes-directory (expand-file-name "~/dotfile/emacs/recipes"))
  (mis-config-default))

(use-package makefile-mode :defer t
  :init
  (add-hook 'makefile-bsdmake-mode-hook #'makefile-gmake-mode))

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (setq markdown-footnote-location 'immediately))

(use-package move-text :ensure t
  :commands
  (move-text-down
   move-text-up))

(use-package multiple-cursors
  :ensure t
  :bind*
  (("M-s-s" . mc/mark-previous-like-this)
   ("M-s-t" . mc/mark-next-like-this)
   ("M-s-S" . mc/unmark-next-like-this)
   ("M-s-T" . mc/unmark-previous-like-this)
   ("H-SPC" . hydra-mc/body))
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

(use-package mu4e
  :bind* (("<f5>" . mu4e))
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :init

  (add-hook! 'message-mode-hook
    (turn-on-orgtbl)
    (turn-on-orgstruct++)
    (set-local-company-backends! 'company-capf)
    (company-mode))

  :config
  (use-package smtpmail)

  ;; use mu4e as emacs default mail program
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-attachment-dir "~/Downloads/")
  (setq mu4e-maildir (expand-file-name "~/Maildir/")
        mu4e-drafts-folder "/drafts"
        mu4e-sent-folder "/sent"
        mu4e-trash-folder "/trash"
        mu4e-get-mail-command "mbsync -a"
        mu4e-html2text-command "w3m -I UTF-8 -O UTF-8 -T text/html"
        mu4e-headers-auto-update t
        mu4e-use-fancy-chars nil
        mu4e-confirm-quit nil
        mu4e-compose-format-flowed nil)

  ;; does not include myself when replying
  (setq mu4e-compose-dont-reply-to-self t)

  (setq mu4e-maildir-shortcuts
        '(("/gmail/inbox" . ?i)
          ("/univ/inbox"  . ?u)
          ("/sent"        . ?s)
          ("/trash"       . ?t)
          ("/drafts"      . ?d)))

  (setq mu4e-bookmarks
        `(("date:today..now AND NOT flag:trashed" "Today" ,?t)
          ("flag:unread AND NOT flag:trashed" "Unread" ,?s)
          ("date:7d..now AND NOT flag:trashed" "Week" ,?r)
          ("maildir:/univ/inbox" "Univ" ,?n)
          ("maildir:/gmail/inbox" "Gmail" ,?m)))

  (setq mu4e-compose-reply-to-address "samuel.barreto8@gmail.com"
        user-mail-address "samuel.barreto8@gmail.com"
        user-full-name "Samuel Barreto")

  ;; send message
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/local/bin/msmtp")
  ;; tell msmtp to choose the smtp server according to the from field
  (setq message-send-mail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)

  ;; show images
  (setq mu4e-show-images t
        mu4e-view-show-images t)

  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; view mail in browser if possible
  (add-to-list
   'mu4e-view-actions
   '("browser" . mu4e-action-view-in-browser) t)

  ;; attach files from dired
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook! 'dired-mode-hook
    (turn-on-gnus-dired-mode))

  ;; keybindings
  (bind-keys
   :map mu4e-main-mode-map
   ("n" . next-line)
   ("p" . previous-line)
   :map mu4e-compose-mode-map
   ("M-q" . nil)
   :map mu4e-headers-mode-map
   ("s-c" . mu4e-headers-query-prev)
   ("s-r" . mu4e-headers-query-next))

  (defhydra hydra-mu4e
    (:hint nil
     :color blue
     :columns 1)
    "MAIL:"
    ("c" mu4e-compose-new "compose mail")
    ("m" mu4e-headers-search-bookmark "mail")
    ("u" mu4e-update-mail-and-index "update" :color red)
    ("s" mu4e-headers-search "search")))

(use-package mwim :ensure t
  :bind* (("C-a" . mwim-beginning)
          ("C-e" . mwim-end)))

;;;; N

;;;; O

(use-package osx-clipboard
  :ensure t
  :if (and (eq system-type 'darwin)
           (window-system))
  :config
  (osx-clipboard-mode +1))

(use-package osx-dictionary
  :ensure t
  :if (eq system-type 'darwin)
  :commands (osx-dictionary-search-input
             osx-dictionary-search-word-at-point))

(use-package outline
  :diminish (outline-minor-mode . "")
  :commands (outline-hide-body
             outline-show-all
             outline-minor-mode
             hydra-outline/body)
  :hook (prog-mode . outline-minor-mode)
  :config
  (defun outline-narrow-to-subtree ()
    (interactive)
    (outline-mark-subtree)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark))

  (defun outline-indent-subtree ()
    (interactive)
    (save-excursion
      (outline-mark-subtree)
      (indent-region (region-beginning) (region-end))
      (message nil)))

  (bind-key "C-c @ n" 'outline-narrow-to-subtree)
  (bind-key "C-c @ i" 'outline-indent-subtree))

(use-package outshine
  :ensure t
  :hook (outline-minor-mode . outshine-hook-function)
  :bind* (("C-M-i" . outshine-cycle-buffer)))

;;;; P

(use-package pathify :ensure t
  :after dired
  :bind (:map dired-mode-map
         ("L" . pathify-dired))
  :config
  (setq pathify-directory "~/.local/bin"))

(use-package pbcopy :ensure t
  :if (memq window-system '(mac ns))
  :config
  (turn-on-pbcopy))

(use-package polymode
  :ensure t
  :mode ("\\.Rmd\\'" . poly-markdown+R-mode)
  :config
  (use-package poly-markdown
    :ensure t)

  (use-package poly-org
    :ensure t)

  (use-package poly-R
    :ensure t)

  (use-package poly-noweb
    :ensure t))


  :ensure t
  :when window-system
  ;; defer loading after 2 seconds of standing still
  :defer 2
  :config (spaceline-all-the-icons-theme))

(use-package cperl
  :mode ("\\.pl\\'". cperl-mode)
  :defines (cperl-eldoc-documentation-function)
  :init
  (defalias 'perl-mode 'cperl-mode)

  (defun cperl-eldoc-documentation-function ()
    "Return meaningful doc string for `eldoc-mode'."
    (car
     (let ((cperl-message-on-help-error nil))
       (cperl-get-help))))

  (add-hook 'cperl-mode-hook
            (lambda ()
              (set (make-local-variable 'eldoc-documentation-function)
                   'cperl-eldoc-documentation-function)
              (my-pde-load)))

  :config
  (use-package plsense :ensure t
    :config
    (plsense-config-default))

  (use-package pde-load
    :load-path "~/src/github.com/wenbinye/emacs-pde/lisp"
    :defines (my-pde-load)
    :commands (my-pde-load)
    :init
    (setq pde-extra-setting t)

    :config
    (defun my-pde-load ()
      (interactive)
      (load "pde-load")))

  (setq cperl-invalid-face (quote off))
  (setq cperl-invalid-face nil)

  (sp-local-pair 'perl-mode "{" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'perl-mode "\"\"" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET"))))

(use-package pretty-hydra
  :load-path "~/.emacs.d/private/major-mode-hydra"
  :commands (pretty-hydra-define))

(use-package prog-mode
  :config
  (add-hook! 'prog-mode-hook
    (display-line-numbers-mode))

  (global-prettify-symbols-mode))

(use-package projectile :ensure t
  :diminish (projectile-mode . "Ⓟ")
  :bind* (("s-p" . projectile-switch-project))
  :commands (projectile-mode
             projectile-find-file)
  :config
  (projectile-global-mode 1)

  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")

  (use-package counsel-projectile :ensure t
    :commands (counsel-projectile-mode)
    :bind* (("s-p" . counsel-projectile-switch-project)
            ("s-f" . counsel-projectile-find-file)))

  (add-hook! 'projectile-mode-hook
    (counsel-projectile-mode)))

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


;;;; Q

;;;; R

(use-package rainbow-delimiters  :ensure t
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode))

(use-package recentf
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package reftex
  :hook ((org-mode . reftex-mode)
         (TeX-mode . reftex-mode))
  :config
  (setq reftex-default-bibliography
        '("/Users/samuelbarreto/Dropbox/bibliography/references.bib"))
  (setq reftex-cite-format 'biblatex)

  (defun org-mode-link-reftex-entry (key)
    "Returns the address of pdf file registered in the file entry
of KEY."
    (require 'bibtex-completion)
    (car (bibtex-completion-find-pdf-in-field key)))

  (defun org-mode-reftex-setup! ()
    (when (and (buffer-file-name)
               (file-exists-p (buffer-file-name)))
      (reftex-set-cite-format "[[papers:%l][%A \(%t\)]]")
      (add-to-list
       'org-link-abbrev-alist
       '("papers" . "%(org-mode-link-reftex-entry)") t #'equal)))

  (add-hook! 'org-mode-hook
    (org-mode-reftex-setup!))

  ;; auto revert so that reftex updates when the bibtex file is
  ;; updated.
  (add-hook! 'bibtex-mode-hook
    (turn-on-auto-revert-mode)))

(use-package restart-emacs :ensure t
  :commands (restart-emacs))

(use-package rust-mode :ensure t
  :mode ("\\.rs\\'" . rust-mode)
  :config

  (use-package racer :ensure t
    :hook (rust-mode . racer-mode)
    :config
    (add-hook 'racer-mode-hook #'eldoc-mode))

  (use-package cargo :ensure t
    :hook (rust-mode . cargo-minor-mode)))

;;;; S

(use-package selected
  :diminish (selected-minor-mode . "")
  :ensure t
  :defer 1
  :commands (selected-minor-mode
             selected-global-mode)
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  :bind (:map selected-keymap
         ("a" . align-regexp)
         ("e" . er/expand-region)
         ("c" . er/contract-region)
         ("q" . selected-off)
         ("s" . sort-lines)
         ("u" . upcase-region)
         ("d" . downcase-region)
         ("w" . count-words-region)
         ("m" . apply-macro-to-region-lines)
         ("x" . kill-ring-save)
         ("X" . kill-region)
         :map selected-org-mode-map
         ("t" . org-table-convert-region)
         ("s" . org-ensrc!)
         ("q" . org-enquote!))
  :init
  (selected-global-mode)
  :config
  (setq selected-minor-mode-override t)
  (selected-global-mode))

(use-package server
  :defer 2
  :config
  (unless (server-running-p) (server-start)))

(use-package scroll-bar
  :defer 2
  :config
  (scroll-bar-mode -1))

(use-package sh-script :defer t
  :mode
  (("\\.sh\\'" . shell-script-mode)
   ("\\.bash\\'" . shell-script-mode)
   ("\\.zsh\\'" . shell-script-mode)
   ("zlogin\\'" . shell-script-mode)
   ("zlogout\\'" . shell-script-mode)
   ("zpreztorc\\'" . shell-script-mode)
   ("zprofile\\'" . shell-script-mode)
   ("zshenv\\'" . shell-script-mode)
   ("zshrc\\'" . shell-script-mode))
  :config
  (setq shell-file-name "/usr/local/bin/bash")

  (use-package company-shell
    :ensure t
    :config
    (add-hook! 'sh-mode-hook
      (set-local-company-backends! 'company-shell)))

  (defun indent-paragraph ()
    (interactive)
    (save-excursion
      (mark-paragraph) (indent-region (region-beginning) (region-end))))

  (defun sh-cleanup-line ()
    (interactive)
    (let* ((beg (line-beginning-position)))
      (save-excursion
        (end-of-line)
        (while (re-search-backward "--\\||\\|([><])\{1,2\}" beg t)
          (insert "\\")
          (newline-and-indent))
        (indent-paragraph))))


  ;; keybindings
  (general-define-key :keymaps 'sh-mode-map "C-c c" 'sh-cleanup-line)
  (general-define-key :keymaps 'sh-mode-map "C-<return>" 'sh-send-line-or-region-and-step))

(use-package shell
  :functions (shell-dwim)
  :bind ("s-'" . shell-dwim)
  :config
  (setq explicit-shell-file-name "/usr/local/bin/bash")
  (defun shell-dwim ()
    "Opens a shell buffer in directory associated with current
buffer.

If the current buffer is already a shell buffer, it closes the
window or do nothing if shell is the sole current window in
frame.
"

    (interactive)
    (if (string-equal major-mode "shell-mode")
        (ignore-errors (delete-window))
      (let* ((cwd default-directory)
             (buf "*shell*")
             (proper-cwd (shell-quote-argument (expand-file-name cwd))))
        (shell buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (comint-kill-input)
          (insert (concat "cd " proper-cwd))
          (let ((comint-process-echoes t))
            (comint-send-input))
          (recenter 0))))))

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
          ("C-S-f" . sp-forward-symbol)
          ("C-ß"   . sp-splice-sexp))
  :init
  (add-hook! 'after-init-hook
    (smartparens-global-mode))
  (add-hook! 'prog-mode-hook
    (smartparens-strict-mode))

  :config
  ;; Only use smartparens in web-mode
  (sp-local-pair 'text-mode "« " " »" :trigger-wrap "«")
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
  (sp-local-pair 'org-mode "=" "=")
  (sp-local-pair 'text-mode "--- " " ---" :trigger-wrap "—")

  ;; TODO: smartparens search wrap and trigger functions.
  (sp-local-pair 'org-mode "/" "/" :trigger-wrap "/" )
  (sp-local-pair 'org-mode "#+BEGIN_QUOTE\n" "#+END_QUOTE\n" :wrap "M-( q")
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)

  (sp-local-pair 'c-mode  "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'cc-mode "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))



  (defun sam--create-newline-and-enter-sexp (&rest _ignored)
    "Open a new brace or bracket expression, with relevant newlines and indent. "
    ;; from https://github.com/Fuco1/smartparens/issues/80
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode)))

(use-package smooth-scrolling
  :ensure t
  :hook (after-init . smooth-scrolling-mode)
  :config
  (setq smooth-scroll-margin 5))

(use-package solarized-theme
  :ensure t
  :defer t
  :config
  (setq solarized-distinct-fringe-background t) ; make the fringe stand out from the background
  (setq solarized-use-variable-pitch t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-more-italic t)
  (setq solarized-scale-org-headlines t)

  (defun solarized-no-background! ()
    (interactive)
    (set-face-background 'default "unspecified-bg"))

  (cond (window-system
         (load-theme 'solarized-dark t))
        (t
         (load-theme 'solarized-dark t)
         (solarized-no-background!))))

(use-package spotlight :ensure t
  :bind (("C-c C-s" . spotlight)
         ("C-c C-S-s" . spotlight-fast)))

(use-package string-inflection :ensure t
  :bind ("C-x M-s" . hydra-string-inflection/body)
  :config

  (defhydra hydra-string-inflection
    (:color pink
     :columns 3)
    "STRING INFLECTION"
    ("t" string-inflection-toggle  "toggle")
    ("l" string-inflection-lower-camelcase "lower-camelcase")
    ("L" string-inflection-camelcase "camelcase")
    ("i" string-inflection-lisp "lisp")
    ("c" string-inflection-cycle "cycle")
    ("u" string-inflection-upcase "upcase")
    ("a" string-inflection-all-cycle "all-cycle")
    ("k" string-inflection-kebab-case "kebab-case")
    ("u" string-inflection-underscore "underscore")
    ("*" iedit-mode "iedit" :exit nil)
    ("q" nil "quit" :color blue)))

(use-package subword :defer t
  :diminish ""
  :init
  (add-hook! 'prog-mode-hook
    (subword-mode 1)))

(use-package swiper :ensure t
  :bind* ("M-s" . swiper))

;;;; T

(use-package tex-site
  :ensure auctex
  :commands (TeX-tex-mode)
  :mode (("\\.tex\\'" . TeX-tex-mode)
         ("\\.cls\\'" . TeX-tex-mode))
  :config
  (add-hook! 'TeX-mode-hook
    (hl-todo-mode))

  (load-file "~/dotfile/emacs/latex-config.el"))

(use-package tiny :ensure t
  :bind* (("C-;" . tiny-expand)))

(use-package tooltip
  :defer 2
  :config
  (tooltip-mode -1))

(use-package tool-bar
  :defer 2
  :config
  (tool-bar-mode -1))

;;;; U

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "")
  :functions (sam|undo
              sam|undo-tree-visualize
              sam|redo)
  :bind* (("C-x u" . sam|undo-tree-visualize)
          ("C-z" . sam|undo)
          ("C-S-z" . sam|redo))
  :config
  (defun sam|undo (&optional arg)
    (interactive)
    (undo-tree-mode t)
    (undo-tree-undo))

  (defun sam|undo-tree-visualize ()
    (interactive)
    (undo-tree-mode t)
    (undo-tree-visualize))

  (defun sam|redo (&optional arg)
    (interactive)
    (undo-tree-mode t)
    (undo-tree-redo))

  (general-define-key
   :keymaps 'undo-tree-visualizer-mode-map
   "RET" 'undo-tree-visualizer-quit
   "t" 'undo-tree-visualize-redo
   "s" 'undo-tree-visualize-undo
   "c" 'undo-tree-visualize-switch-branch-left
   "r" 'undo-tree-visualize-switch-branch-right))

;;;; V

(use-package visual-regexp-steroids :ensure t
  :bind* (("s-%" . vr/replace)
          ("M-s-%" . vr/query-replace))
  :config
  (setq vr/auto-show-help nil))

(use-package visual-fill-column :ensure t
  :config
  (global-visual-fill-column-mode +1)
  (toggle-word-wrap +1))

;;;; W

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
  (use-package company-web :ensure t)

  (add-hook! 'web-mode-hook
    (set-local-company-backends! 'company-web-html)
    (setq-local tab-width 2)
    (setq-local outline-regexp "<!--*")))

(use-package wgrep :ensure t :defer t)

(use-package which-key
  :ensure t
  :defer 2
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; simple then alphabetic order.
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-popup-type 'side-window
        which-key-side-window-max-height 0.3
        which-key-side-window-max-width 0.5
        which-key-idle-delay 0.3
        which-key-min-display-lines 7)
  ;; key description for C-x
  (which-key-add-key-based-replacements
    "C-x RET" "coding system -input"
    "C-x 4"   "Other Window"
    "C-x 5"   "Frame"
    "C-x 6"   "2C"
    "C-x @"   "event"
    "C-x 8"   "special char"
    "C-x a"   "abbrev"
    "C-x n"   "narrow"
    "C-x r"   "rectangle"
    "C-x v"   "version control"
    "C-c &"   "yas"
    "C-c @"   "hide-show"
    "M-SPC h" "info"
    "M-SPC g" "grep"
    "M-SPC M-s" "occur"))

(use-package wiki-summary
  :ensure t

;;;; X

(use-package xterm-color
  :ensure t
  :config
  ;; comint install
  (add-hook! 'comint-preoutput-filter-functions
    (xterm-color-filter))
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

;;;; Y

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


;;;; Z

;;; personal functions

(load-file "~/dotfile/emacs/functions.el")

;;; org

(load-file "~/dotfile/emacs/org.el")
(load-file "~/dotfile/emacs/ess-config.el")

;;; bind-key* "C-x x"  keybindings

(load-file "~/dotfile/emacs/keybindings.el")

;;; custom

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; TODO: check out https://www.youtube.com/watch?v=m_Oj_6Bjryw
;; TODO: check out https://github.com/Kungsgeten/selected.el
;; TODO: check out https://vxlabs.com/2017/02/07/mu4e-0-9-18-e-mailing-with-emacs-now-even-better/
;; TODO: check out https://github.com/atgreen/paperless
;; TODO: check out https://github.com/masasam/emacs-easy-hugo

