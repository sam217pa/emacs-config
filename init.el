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

;;; Package.el

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          (lambda ()
            ;; restore after startup
            (setq gc-cons-threshold 800000)))


(setq package-enable-at-startup nil)
(let ((default-directory "~/.emacs.d/elpa"))
  (normal-top-level-add-subdirs-to-load-path))
(package-initialize t)
(require 'use-package)

(setq package-check-signature nil)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package general :ensure t)
(use-package diminish)
(use-package bind-key)

(use-package server
  :config
  (unless (server-running-p) (server-start)))

;;; Sane default
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
 coding-system-for-read 'utf-8        ; use UTF8 pour tous les fichiers
 coding-system-for-write 'utf-8       ; idem
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

(defvar my-font-for-light "Fira Code 10"
  "Font for coding situations")

(defvar my-font-for-text "Input Serif Narrow 12"
  "Font for text")

;;; apparences
(when window-system
  (tooltip-mode -1)                    ; don't know what that is
  (tool-bar-mode -1)                   ; sans barre d'outil
  (menu-bar-mode -1)                   ; barre de menu
  (scroll-bar-mode -1)                 ; enlève la barre de défilement
  (blink-cursor-mode -1)               ; pas de clignotement
  (set-frame-size (selected-frame) 85 61)
  (add-to-list 'default-frame-alist '(height . 46))
  (add-to-list 'default-frame-alist '(width . 85))
  (fringe-mode '(4 . 0))                ; reduce fringe size to 4 px
  (setq-default line-spacing 6)         ; increase between-line space.

  ;; change default font for current frame
  (add-to-list 'default-frame-alist `(font . ,my-font-for-light))
  (set-face-attribute 'default nil :font my-font-for-light)

  (defun simple-mode-line-render (left right)
    "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
    (let* ((available-width (- (window-width) (length left) 2)))
      (format (format " %%s %%%ds " available-width) left right)))

  ;; use the function in conjunction with :eval and format-mode-line in
  ;; your mode-line-format
  (setq-default mode-line-format
                '((:eval (simple-mode-line-render
                          ;; left
                          (format-mode-line "%*%* %b       [%m]")
                          ;; right
                          (concat (format-mode-line "%l")
                                  (when (bound-and-true-p eyebrowse-mode)
                                    (concat " " (eyebrowse-mode-line-indicator)))))))))

;;; keybindings

(when (eq system-type 'darwin)           ; mac specific bindings
  (setq mac-right-command-modifier 'meta ; cmd de droite = meta
        mac-command-modifier 'control    ; cmd de gauche = control
        mac-option-modifier 'super       ; option de gauche = super
        mac-right-option-modifier nil ; option de droite = carac spéciaux
        mac-control-modifier 'hyper ; control de gauche = hyper (so does capslock)
        ns-function-modifier 'hyper ; fn key = hyper
        ns-right-alternate-modifier nil) ; cette touche n'existe pas.
  (setq mac-pass-command-to-system nil) ; disable system call to commands like
                                        ; C-h (hide frame on macOS by default
  (setq mac-pass-control-to-system nil) ; idem
  (setq-default locate-command "mdfind")



  (setq delete-by-moving-to-trash t)
  (defun system-move-file-to-trash (file)
    "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash") nil nil nil file))
  ;; (mac-auto-operator-composition-mode t)
  ) ; enable ligatures


;;; Packages


;;;; A

(use-package abbrev :defer t
  :diminish ""
  :init
  (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))
  ;; tell emacs where to read abbrev definitions from...
  (setq abbrev-file-name "~/dotfile/emacs/.abbrev_defs")
  (setq save-abbrevs 'silently)
  (setq only-global-abbrevs t)
  (setq-default abbrev-mode t))

(use-package ace-window :ensure t
  :commands
  ace-window
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
  :delight
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

(use-package autorevert :defer t
  ;; mainly to make autorevert disappear from the modeline
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

(use-package bash-completion :ensure t
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete))

;; (use-package base16-theme :ensure t
;;   :demand t)

(use-package bioseq-mode
  :load-path "~/.emacs.d/private/bioseq-mode"
  :commands (bioseq-mode))

(use-package blank-mode :ensure t
  :commands blank-mode)



;;;; C

(use-package cc-mode :ensure t
  :defer t
  :config
  (use-package company-c-headers :ensure t)

  (use-package cquery :ensure t
    :after lsp-mode
    :commands (lsp-cquery-enable)
    :init

    (defun cquery//enable ()
      (ignore-errors (lsp-cquery-enable)))

    (add-hook 'c-mode-common-hook #'cquery//enable)
    :config
    (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
    (setq cquery-extra-init-params '(:completion (:detailedLabel t)))
    (setq cquery-sem-highlight-method 'font-lock)
    (setq cquery-executable "/usr/local/bin/cquery"))

  ;; (use-package irony :ensure t
  ;;   :init (add-hook 'c-mode-hook 'irony-mode)
  ;;   :config
  ;;   (use-package company-irony :ensure t
  ;;     :after company
  ;;     :config
  ;;     (add-hook 'c-mode-hook
  ;;               (lambda ()
  ;;                 (setq-local company-backends
  ;;                             (append '(company-irony) company-backends))))))

  (use-package ggo-mode :ensure t
    :mode ("\\.ggo\\'" . ggo-mode))

  (defun compile-file ()
    "Runs the compilation of the current file.
Assumes it has the same name, but without an extension"
    (interactive)
    (compile (file-name-sans-extension buffer-file-name))))


(use-package conf-mode
  :mode (("DESCRIPTION" . conf-mode)
         ("\\.log\\'" . conf-mode)
         ("\\.toml\\'" . conf-toml-mode)))

(use-package command-log-mode :ensure t
  :commands (command-log-mode))

(use-package company :ensure t
  :diminish ""
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)

  (setq company-tooltip-align-annotations t)
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
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-clang-arguments '("-I /usr/local/Cellar/guile/2.2.3_1/include/guile/2.2/"))

  (use-package company-statistics
    :ensure t
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

(use-package company-childframe
  :load-path "~/.emacs.d/private/company-childframe"
  :after posframe
  :config
  (company-childframe-mode t)
  (require 'desktop)
  (push '(company-childframe-mode . nil) desktop-minor-mode-table))

(use-package company-bibtex
  :ensure t
  :after company
  :config
  (setq company-bibtex-bibliography '("~/Dropbox/bibliography/references.bib"))
  (add-hook 'latex-mode-hook
            (lambda ()
              (add-to-list 'company-backends 'company-bibtex))))

(use-package counsel :ensure
  :commands (counsel-load-theme
             counsel-bookmark)
  :bind* (("M-x" . counsel-M-x)
          ("C-x C-f" . counsel-find-file)
          ("s-<backspace>" . ivy-switch-buffer)
          ("C-c i" . counsel-imenu)
          ("C-c /" . counsel-rg))

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

(use-package csv-mode :ensure t
  :mode (("\\.csv\\'" . csv-mode))
  :bind (:map csv-mode-map
         ("'" . hydra-csv/body))
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
    ("q" nil "quit" :color blue))

  (setq csv-invisibility-default nil)

  (defun csv--align-buffer ()
    (save-excursion
      (csv-align-fields t (point-min) (point-max))))

  (defun csv--next-or-new-field ()
    (cond ((looking-at ",$")
           (forward-char 1)
           (save-excursion (insert ",")))
          ((eq (point) (point-at-eol))
           (insert ","))
          (t
           (unless (re-search-forward "," nil t)
             (end-of-line))))
    (csv--align-buffer))

  (defun csv-tab-to-next-field ()
    (interactive)
    (if (or (mapcar #'looking-at csv-separators))
        (csv--next-or-new-field)
      (yas-expand)))

  (defun csv--previous-field ()
    (re-search-backward "," nil t))

  (defun csv-backtab-to-previous-field ()
    (interactive)
    (when (or (mapcar #'looking-at csv-separators))
      (csv--previous-field)))

  (defun csv--new-line-or-next-field ()
    (cond ((and (looking-back "," (- (point) 2))
                (eq (point) (point-at-eol)))
           (delete-char -1)
           (unless (re-search-forward "," nil t)
             (newline)))
          (t
           (if (eq (point) (point-max))
               (newline)
             (next-line))))
    (csv--align-buffer))

  (defun csv-new-line-or-next-field ()
    (interactive)
    (when (mapcar #'looking-at csv-separators)
      (csv--new-line-or-next-field)))

  (general-define-key
   :keymaps 'csv-mode-map
    "<tab>" 'csv-tab-to-next-field
    "<backtab>" 'csv-backtab-to-previous-field
    "RET" 'csv-new-line-or-next-field))


;;;; D

(use-package dired
  :bind* (("C-x d" . dired-other-window)
          ("C-x C-d" . dired))
  :commands (dired)
  :config
  (use-package dired-x
    :bind* (("C-x C-'" . dired-jump))
    :commands (dired-omit-mode)
    :init
    (add-hook 'dired-load-hook (lambda () (load "dired-x")))
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    :config
    (setq dired-omit-verbose nil)
    (setq dired-omit-extensions
          (cl-list* ".aux" ".fls" ".log" ".out" ".toc" ".fdb_latexmk" ".bbl" ".blg" ".bcf" ".run.xml" ".x" ".d" dired-omit-extensions))
    (setq dired-omit-files (concat dired-omit-files
                                   "\\|^\\..*$\\|^.DS_Store$\\|^.projectile$\\|^.git$")))


  (use-package dired-details :ensure t
    :disabled t
    :config
    (dired-details-install)
    (setq-default dired-details-hidden-string " --- "
                  dired-details-hide-link-targets nil))

  (use-package dired-sort :ensure t
    :defines (hydra-dired-sort/body)
    :commands (hydra-dired-sort/body
               dired-sort-name
               dired-sort-size
               dired-sort-time
               dired-sort-ctime
               dired-sort-utime
               dired-sort-extension)
    :config
    (defhydra hydra-dired-sort
      (:color blue
       :hint nil
       :post (hydra-dired-main/body))
      "
_n_ame
_s_ize
_t_ime
_e_xtension"
      ("n" dired-sort-name)
      ("s" dired-sort-size)
      ("t" dired-sort-time)
      ("e" dired-sort-extension)))


  ;;preview files in dired
  (use-package peep-dired
    :ensure t
    :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
    :bind (:map dired-mode-map
           ("P" . peep-dired)))

  ;;narrow dired to match filter
  (use-package dired-narrow
    :ensure t
    :bind (:map dired-mode-map
           ("/" . dired-narrow)))

  (use-package dired-quick-sort :ensure t
    ;; press S in dired to see a nice hydra for sorting
    :config
    (dired-quick-sort-setup))


  (let ((gls "/usr/local/bin/gls"))
    (if (file-exists-p gls)
        (setq insert-directory-program gls)))

  ;; (setq ls-lisp-use-insert-directory-program nil)
  ;; (setq dired-listing-switches "-alF")
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


;;;; E

(use-package ediff
  :commands (ediff
             ediff3)
  :config
  (setq ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package eldoc :ensure t
  :commands turn-on-eldoc-mode
  :diminish ""
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode))

(use-package elfeed :ensure t
  :bind* ("C-c E" . elfeed)
  :config

  ;; increase title width to accomodate papers
  (setq elfeed-search-title-max-width 120)

  (use-package elfeed-org
    :load-path "~/.emacs.d/private/elfeed-org"
    :config
    (setq rmh-elfeed-org-files (list "~/dotfile/emacs/elfeed.org"))
    (elfeed-org))

  (defun elfeed-mark-all-as-read ()
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (bind-keys
   :map elfeed-search-mode-map
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

  (defhydra hydra-elfeed (:color pink :hint nil :columns 3)
    "
^ACTIONS^     ^ ^          ^MARK^       ^TAG^
_u_: update   _b_: browse  _r_: read    _+_: add
_g_: fetch    _y_: yank    _u_: unread  _-_: remove
_s_: search
"
    ("u" elfeed-search-update--force)
    ("g" elfeed-search-fetch)
    ("s" elfeed-search-live-filter)
    ("b" elfeed-search-browse-url)
    ("y" elfeed-search-yank)
    ("r" elfeed-search-untag-all-unread)
    ("u" elfeed-search-tag-all-unread)
    ("+" elfeed-search-tag-all)
    ("-" elfeed-search-untag-all)
    ("N" elfeed-next-tag "next tag" :color red)
    ("q" (message "Abort.") "Quit" :color blue))

  (setq-default elfeed-search-filter "@6-months-ago +unread"))

(use-package emacs-lisp-mode
  :mode
  (("*scratch*" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local lisp-indent-function #'Fuco1/lisp-indent-function)
              (setq-local company-backends
                          (append '(company-elisp) company-backends))))

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

(use-package eshell
  :defines eshell-here
  :commands (eshell eshell-here)
  :config
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t
        eshell-list-files-after-cd t
        eshell-ls-initial-args "-alh")

  ;; TODO: pimp to base16
  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize "┌─[" 'face `(:foreground "green"))
           (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))
           (propertize "]──[" 'face `(:foreground "green"))
           (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
           (propertize "]\n" 'face `(:foreground "green"))
           (propertize "└─>" 'face `(:foreground "green"))
           (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))
           )))

  (setq eshell-directory-name "~/dotfile/emacs/eshell/")

  ;; from http://www.howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
    "
Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier.
"
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))

  (defun eshell/ll ()
    (insert "ls -l")
    (eshell-send-input))

  (setq eshell-prompt-function (lambda () (concat "$ ")))
  (setq eshell-prompt-function
        (lambda ()
          (let ((green (plist-get base16-shell-colors-256 :base0B))
                (white (plist-get base16-shell-colors-256 :base07))
                (yellow (plist-get base16-shell-colors-256 :base0F)))
            (concat
             (propertize "─[" 'face `(:foreground ,green))
             (propertize (concat (eshell/pwd)) 'face `(:foreground ,white))
             (propertize "]──[" 'face `(:foreground ,green))
             (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground ,yellow))
             (propertize "]\n" 'face `(:foreground ,green))
             (propertize (if (= (user-uid) 0) " # " "> ") 'face `(:foreground ,green))))))

  ;; should bind after eshell starts because eshell-mode-map is lazily
  ;; defined. see https://emacs.stackexchange.com/questions/27849
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-smart-initialize)
              (eshell-cmpl-initialize)
              ;; init plan-9 like shell
              (define-key eshell-mode-map (kbd "s-'") 'delete-window)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))))

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
  (add-hook 'ess-mode-hook
            (lambda ()
              (smartparens-mode 1)
              (lesspy-mode 1)
              (run-hooks 'prog-mode-hook 'company-mode-hook)))
  (add-hook 'inferior-ess-mode-hook
            (lambda ()
              (setq-local outline-regexp "^>")
              (rainbow-mode t)))
  (setq ess-offset-continued 2           ; offset after first statement
        ess-expression-offset 2          ; offset for expression
        ess-nuke-trailing-whitespace-p t ;delete trailing whitespace
        ess-default-style 'RStudio) ; set default style for R source file

  (setq ess-indent-with-fancy-comments nil)
  ;; do not truncate line in the R repl:
  (add-hook 'inferior-ess-mode-hook (lambda () (toggle-truncate-lines 1)))
  (add-hook 'ess-julia-mode-hook #'ess-roxy-mode)
  (add-hook 'ess-julia-mode-hook #'latex-unicode-mode)
  :config
  ;; config for R
  (load-file "~/dotfile/emacs/ess-config.el")

  (setq inferior-julia-program-name "/usr/local/bin/julia")

  (bind-keys :map julia-mode-map
    ("RET" . (lambda () (interactive) (ess-roxy-newline-and-indent)))))

(use-package esup :ensure t
  :commands esup)

(use-package etags-select :ensure t
  :defines (sam-find-tag
            build-ctags
            visit-project-tags)
  :bind* ("M-s-." . sam-find-tag)
  :commands (build-ctags
             visit-project-tags)
  :config

  ;; from http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
  (defun build-ctags ()
    (interactive)
    (message "building project tags")
    (let ((root (projectile-project-root)))
      (shell-command (concat "ctags -e -R --extra=+fq --exclude=bin --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
    (visit-project-tags)
    (message "tags built successfully"))

  (defun visit-project-tags ()
    (interactive)
    (let ((tags-file (concat (projectile-project-root) "TAGS")))
      (visit-tags-table tags-file)
      (message (concat "Loaded " tags-file))))

  (defun sam-find-tag ()
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "TAGS"))
        (visit-project-tags)
      (build-ctags))
    (etags-select-find-tag-at-point)))

(use-package exec-path-from-shell :ensure t
  :if (memq window-system '(mac ns))
  :defer 2
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

(use-package eyebrowse          ; Easy workspaces creation and switching
  :ensure t
  :defines hydra-eyebrowse/body
  :bind* (("C-c w" . hydra-eyebrowse/body)
          ("H-r" . eyebrowse-next-window-config)
          ("H-c" . eyebrowse-prev-window-config))
  :config
  (defhydra hydra-eyebrowse (:color blue :hint nil)
    "
Workspace: _1_ _2_ _3_ _4_ _5_ _6_ _7_ _8_ _9_ _0_
Navigate : _n_ext _p_rev _c_lose _l_ast
"
    ("1" eyebrowse-switch-to-window-config-1)
    ("2" eyebrowse-switch-to-window-config-2)
    ("3" eyebrowse-switch-to-window-config-3)
    ("4" eyebrowse-switch-to-window-config-4)
    ("5" eyebrowse-switch-to-window-config-5)
    ("6" eyebrowse-switch-to-window-config-6)
    ("7" eyebrowse-switch-to-window-config-7)
    ("8" eyebrowse-switch-to-window-config-8)
    ("9" eyebrowse-switch-to-window-config-9)
    ("0" eyebrowse-switch-to-window-config-0)
    ("n" eyebrowse-next-window-config :color red)
    ("p" eyebrowse-prev-window-config :color red)
    ("c" eyebrowse-close-window-config :color red)
    ("l" eyebrowse-last-window-config))

  (setq eyebrowse-mode-line-separator " "
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

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

(use-package flycheck :ensure t
  :commands flycheck-mode
  :diminish (flycheck-mode . "ⓕ")
  :config
  (setq flycheck-highlighting-mode 'symbols))

;;;; G

(use-package geiser
  :mode (("\\.scm\\'" . scheme-mode))
  :hook (scheme-mode . geiser-mode)
  :commands (geiser-mode
             connect-to-guile
             geiser-connect
             geiser)
  :init
  (setq geiser-active-implementations '(guile))
  (setq geiser-guile-load-path '("/usr/local/Cellar/guile/2.2.2/share/guile/"))
  (setq geiser-guile-load-init-file-p t)
  (setq geiser-guile-manual-lookup-nodes '("Guile" "guile-2.2"))
  (setq geiser-guile-binary "/usr/local/bin/guile")
  :config
  (require 'geiser-install))

(use-package ggtags :ensure t
  :commands (ggtags-mode))

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
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

(use-package highlight-indent-guides :ensure t
  :config
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package htmlize :ensure t :defer t)

(use-package hungry-delete :ensure t
  :diminish ""
  :config
  (global-hungry-delete-mode))

(use-package hydra :ensure t
  :config
  (setq hydra-is-helpful t))

(use-package hyperbole :ensure t
  :bind* (("s-<return>" . hkey-either)
          ("s-h" . hyperbole)
          ("C-h h" . hyperbole))
  :mode ("\\.kotl\\'" . kotl-mode))

;;;; I

(use-package ibuffer :ensure t
  :commands ibuffer
  :init
  (add-hook 'ibuffer-hook (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))
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
                   ("shell" (or (mode . eshell-mode)
                                (mode . shell-mode)))
                   ("Music" (or (mode . bongo-mode)
                                (mode . bongo-library-mode)
                                (mode . bongo-playlist-mode)))
                   ("Helm" (or (mode . helm-major-mode)))
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

(use-package indent-tools :ensure t
  :bind (("C-c C-i" . indent-tools-hydra/body)))

(use-package info
  :mode (("\\.info\\'" . Info-mode))
  :config

  (info-initialize)

  (defun sam--list-info-dirs ()
    (let* ((dir "/usr/local/share/info")
           (files (directory-files dir t))
           (links (seq-filter #'file-symlink-p files))
           (links-full (seq-map #'file-truename links ))
           (dirs (remove-duplicates
                  (seq-map #'file-name-directory links-full)
                  :test #'equal)))
      dirs))

  (define-key Info-mode-map (kbd ".") #'hydra-info/body)

  (defhydra hydra-info (:color pink
                        :hint nil)
    "
Info-mode:

  ^^_]_ forward  (next logical node)       ^^_l_ast (←)        _u_p (↑)                             _f_ollow reference       _T_OC
  ^^_[_ backward (prev logical node)       ^^_r_eturn (→)      _m_enu (↓) (C-u for new window)      _i_ndex                  _d_irectory
  ^^_n_ext (same level only)               ^^_H_istory         _g_oto (C-u for new window)          _,_ next index item      _c_opy node name
  ^^_p_rev (same level only)               _<_/_t_op           _b_eginning of buffer                virtual _I_ndex          _C_lone buffer
  regex _s_earch (_S_ case sensitive)      ^^_>_ final         _e_nd of buffer                      ^^                       _a_propos

  _1_ .. _9_ Pick first .. ninth item in the node's menu.

"
    ("]"   Info-forward-node)
    ("["   Info-backward-node)
    ("n"   Info-next)
    ("p"   Info-prev)
    ("s"   hydra-info-search/body :color blue)
    ("S"   Info-search-case-sensitively)

    ("l"   Info-history-back)
    ("r"   Info-history-forward)
    ("H"   Info-history)
    ("t"   Info-top-node)
    ("<"   Info-top-node)
    (">"   Info-final-node)

    ("u"   Info-up)
    ("^"   Info-up)
    ("m"   Info-menu)
    ("g"   Info-goto-node)
    ("b"   beginning-of-buffer)
    ("e"   end-of-buffer)

    ("f"   Info-follow-reference)
    ("i"   Info-index)
    (","   Info-index-next)
    ("I"   Info-virtual-index)

    ("T"   Info-toc)
    ("d"   Info-directory)
    ("c"   Info-copy-current-node-name)
    ("C"   clone-buffer)
    ("a"   info-apropos)

    ("1"   Info-nth-menu-item)
    ("2"   Info-nth-menu-item)
    ("3"   Info-nth-menu-item)
    ("4"   Info-nth-menu-item)
    ("5"   Info-nth-menu-item)
    ("6"   Info-nth-menu-item)
    ("7"   Info-nth-menu-item)
    ("8"   Info-nth-menu-item)
    ("9"   Info-nth-menu-item)

    ("?"   Info-summary "Info summary")
    ("h"   Info-help "Info help")
    ("q"   Info-exit "Info exit")
    ("C-g" nil "cancel" :color blue))

  (defhydra hydra-info-search (:hint nil)
    "search"
    ("s" Info-search "search")
    ("n" Info-search-next "next")
    ("p" Info-search-backward "prev")))

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

(use-package ivy :ensure t
  :config
  (setq ivy-height 20)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t) )

(use-package ivy-bibtex :ensure t
  :bind ("C-x M-b" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq bibtex-completion-library-path '("~/zotero_bib/"))
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-path "~/dotfile/bibliographie/notes.org")
  (setq helm-bibtex-pdf-open-function #'helm-open-file-with-default-tool)
  (setq bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))))

(use-package ivy-posframe
  :after posframe
  :load-path "~/.emacs.d/private/ivy-posframe"
  :config
  (ivy-posframe-setup)
  (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
  (setq ivy-display-function #'ivy-posframe-display-at-point)
  (setq ivy-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

;;;; L

(use-package latex-unicode-math-mode :ensure t
  :commands (latex-unicode-math-mode
             latex-unicode-mode)
  :config
  (use-package company-math :ensure t
    :init
    (add-hook 'ess-julia-mode-hook
              (lambda () (setq-local company-backends
                                (append '((company-math-symbols-latex))
                                        company-backends))))))

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
  ;; :disabled t
  :diminish (lispy-mode . " λ")
  :commands lispy-mode
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lisp-mode-hook (lambda () (lispy-mode 1)))

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

(use-package lsp-mode
  :ensure t
  :config

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)

  (use-package company-lsp
    :after company
    :ensure t
    :config
    (push 'company-lsp company-backends))

  (use-package lsp-ui
    :ensure t
    :config
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)
    (add-hook 'lsp-ui-mode   #'lsp-ui-peek-mode)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (define-key lsp-ui-mode-map (kbd "M-p") #'lsp-ui-find-prev-reference)
    (define-key lsp-ui-mode-map (kbd "M-n") #'lsp-ui-find-next-reference)))

(use-package lua-mode :ensure t
  :mode ("\\.lua\\'" . lua-mode))

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

  (setq magit-completing-read-function 'ivy-completing-read))

(use-package makefile-mode :defer t
  :init
  (add-hook 'makefile-bsdmake-mode-hook #'makefile-gmake-mode))

(use-package markdown-mode :ensure t
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (setq markdown-footnote-location 'immediately)

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
   "gs" 'markdown-up-heading))

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
  :commands (mu4e
             mu4e-new-frame)
  :defines (hydra-mu4e/body
            mu4e-new-frame)
  :bind* (("C-c M" . hydra-mu4e/body))
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :init
  ;; (add-hook 'mu4e-compose-mode-hook (lambda () (openwith-mode -1)))
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  (add-hook 'message-mode-hook
            (lambda () (add-to-list 'company-backends 'company-capf)))
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
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  (defun mu4e-new-frame ()
    (interactive)
    (make-frame)
    (mu4e))

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

(use-package mwim :ensure t
  :bind* (("C-a" . mwim-beginning)
          ("C-e" . mwim-end)))


;;;; N

(use-package nlinum :ensure t
  :commands (global-nlinum-mode
             nlinum-mode)
  :config
  (setq nlinum-highlight-current-line t))

(use-package nov :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (defun my-nov-font-setup ()
    (face-remap-add-relative
     'variable-pitch
     :family "Input Sans Narrow"
     :height 1.0))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  (setq nov-text-width 80))

;;;; O

(use-package osx-clipboard :ensure t
  :if (and (eq system-type 'darwin)
           (window-system))
  :config
  (osx-clipboard-mode +1))

(use-package openwith :ensure t
  :config
  (openwith-mode -1)
  (setq openwith-associations
        '(("\\.xlsx\\'" "open -a Microsoft\\ Excel" (file))
          ("\\.xls\\'" "open -a Microsoft\\ Excel" (file))
          ("\\.doc\\'" "open -a Microsoft\\ Word" (file))
          ("\\.docx\\'" "open -a Microsoft\\ Word" (file)))))

(use-package outline
  :delight
  :bind (("H-<tab>" . hydra-outline/body))
  :commands (outline-hide-body
             outline-show-all
             outline-minor-mode
             hydra-outline/body)
  :config
  (use-package navi-mode :ensure t)
  (use-package outorg :ensure t)
  (use-package outshine :ensure t)
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
  (setq-default outline-minor-mode-prefix (kbd "s-*"))

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

  (bind-key* "C-M-i" #'outshine-cycle-buffer)
  (bind-key "C-c @ n" 'outline-narrow-to-subtree)
  (bind-key "C-c @ i" 'outline-indent-subtree)

  (defhydra hydra-outline
    (:hint nil :body-pre (outline-minor-mode 1))
    "
Outline

^CURRENT^    ^ALL^      ^LEAVES^      ^MANIPULATE^
_c_: hide    _C_: hide  _C-c_: hide    _M-r_: demote
_t_: next    ^ ^        _C-r_: show    _M-c_: promote
_s_: prev    ^ ^        ^   ^          _M-t_: move down
_r_: show    _R_: show  ^   ^          _M-s_: move up
"
    ("C" outline-hide-body)
    ("t" outline-next-visible-heading)
    ("s" outline-previous-visible-heading)
    ("R" outline-show-all)
    ("c" outline-hide-subtree)
    ("r" outline-show-subtree)

    ("C-c" outline-hide-leaves)
    ("C-r" outline-show-subtree)

    ("M-r" outline-demote)
    ("M-c" outline-promote)
    ("M-t" outline-move-subtree-down)
    ("M-s" outline-move-subtree-up)

    ("i" outline-insert-heading "insert heading" :color blue)
    ("q" nil "quit" :color blue)))

(use-package org-download :ensure t
  :after org
  :config

  (defvar sam|org-download-dir "./img/"
    "Default folder to place `org-download' captures in.")

  (defun sam|img-dir ()
    (let (target sam|org-download-dir)
      (cond ((file-directory-p target) target)
            (t (make-directory target) target))))

  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir sam|org-download-dir)
  (when (eq system-type 'darwin)
    (setq-default org-download-screenshot-method "screencapture -i %s"))

  (general-define-key
   :keymaps 'org-mode-map
   :prefix "C-c y"
   "e" 'org-download-edit
   "i" 'org-download-image
   "s" 'org-download-screenshot
   "y" 'org-download-yank
   "k" 'org-download-delete))

;;;; P

(use-package paradox :ensure t
  :commands (paradox-list-packages
             package-list-packages))

(use-package pathify :ensure t
  :bind (:map dired-mode-map
         ("L" . pathify-dired))
  :config
  (setq pathify-directory "~/.local/bin"))

(use-package pbcopy :ensure t
  :if (memq window-system '(mac ns))
  :config
  (turn-on-pbcopy))

(use-package pdf-tools :ensure t
   :mode ("\\.pdf\\'" . pdf-view-mode)
   :config
   (setq pdf-tools-enabled-modes
         '(pdf-isearch-minor-mode
           pdf-links-minor-mode
           pdf-misc-minor-mode
           pdf-outline-minor-mode
           pdf-misc-size-indication-minor-mode
           pdf-misc-menu-bar-minor-mode
           pdf-sync-minor-mode
           pdf-misc-context-menu-minor-mode
           pdf-cache-prefetch-minor-mode
           pdf-view-auto-slice-minor-mode))
   (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
   (require 'pdf-occur)
   (require 'pdf-links)
   (require 'pdf-outline)
   (require 'pdf-sync)
   (pdf-tools-install)

   (setq pdf-view-continuous t)

   (defun pdf-view-open-in-external-app ()
     (interactive)
     (shell-command
      (format
       "open -a Preview %s"
       (shell-quote-argument (buffer-file-name)))))

   (defun pdf-view-finder ()
     (interactive)
     (shell-command "open -a Finder ./"))

   (defun pdf-kill-this-buffer (really-kill)
     (interactive (list (y-or-n-p "Really kill this buffer [yN] ? ")))
     (when really-kill
       (kill-this-buffer)))

   (defhydra hydra-pdf-view ()
     "pdf-view"
     ("t" pdf-view-next-line-or-next-page)
     ("s" pdf-view-previous-line-or-previous-page))

   (defhydra hydra-pdf-view (:hint nil :columns 4 :color pink)
     "
_d_: delete buffer _O_: open extern"
     ("C" pdf-view-first-page "⇚")
     ("c" image-backward-hscroll "←")
     ("r" image-forward-hscroll "→")
     ("R" pdf-view-last-page  "⇛")
     ("ß" pdf-view-previous-page "⇑")
     ("s" pdf-view-previous-line-or-previous-page "↑")
     ("t" pdf-view-next-line-or-next-page "↓")
     ("þ" pdf-view-next-page "⇓")
     ("O" pdf-view-open-in-external-app :color blue)
     ("d" pdf-kill-this-buffer :color blue))

   (bind-keys :map pdf-view-mode-map
     ("." . hydra-pdf-view/body)
     ("r" . image-forward-hscroll)
     ("c" . image-backward-hscroll)
     ("O" . pdf-view-open-in-external-app)
     ("s" . pdf-view-previous-line-or-previous-page)
     ("ß" . pdf-view-previous-page)
     ("þ" . pdf-view-next-page)
     ("t" . pdf-view-next-line-or-next-page)
     ("d" . pdf-kill-this-buffer)
     ("C" . pdf-view-first-page)
     ("R" . pdf-view-last-page)))

(use-package posframe
  :load-path "~/.emacs.d/private/posframe"
  :demand t)

(use-package spacemacs-theme
  :ensure t)

(use-package spaceline)

(use-package spaceline-all-the-icons
  :after spaceline
  :ensure t
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

  ;; (use-package perl-completion :ensure t
  ;;   :init
  ;;   (add-hook 'cperl-mode-hook
  ;;             (lambda ()
  ;;               (require 'perl-completion)
  ;;               (perl-completion-mode t))))

  (use-package pde-load
    :load-path "~/src/github.com/wenbinye/emacs-pde/lisp"
    :defines (my-pde-load)
    :commands (my-pde-load)
    :init
    (setq pde-extra-setting t)
    (setq inf-perl-shell-program (expand-file-name "~/anaconda/bin/re.pl"))
    :config
    (defun my-pde-load ()
      (interactive)
      (load "pde-load"))

    (bind-keys :map cperl-mode-map
      ("C-<return>" . inf-perl-send-line)))

  (setq cperl-invalid-face (quote off))
  (setq cperl-invalid-face nil)

  (sp-local-pair 'perl-mode "{" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'perl-mode "\"\"" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET"))))

(use-package pretty-mode :ensure t
  :disabled t
  :commands turn-on-pretty-mode
  :init
  (add-hook 'ess-mode-hook 'turn-on-pretty-mode))

(use-package prog-mode
  :config
  (defun highlight-todos ()
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)
       ("\\<\\(FIXED\\|DONE\\|DEBUGGED\\):" 1 font-lock-string-face t))))
  (add-hook 'prog-mode-hook 'highlight-todos)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
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
  (add-hook 'projectile-mode-hook #'counsel-projectile-mode))

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

(use-package rainbow-mode :ensure t
  :commands (rainbow-mode))

(use-package recentf
  :commands (recentf-mode
             counsel-recentf)
  :config
  (setq recentf-max-saved-items 50))

(use-package restart-emacs :ensure t
  :commands (restart-emacs))

(use-package rg :ensure t
  :commands (rg))

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
  :delight
  :ensure t
  :commands (selected-minor-mode
             selected-global-mode)
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
         ("X" . kill-region))
  :init
  (selected-global-mode)
  :config
  (setq selected-minor-mode-override t)
  (selected-global-mode))

(use-package shackle :ensure t
  :config
  (shackle-mode t)

  (setq helm-display-function 'pop-to-buffer)

  (setq shackle-rules
        '((calendar-mode :select t :align below :size 0.25)
          (help-mode :select t :align right :size 0.5)
          (compilation-mode :select t :align right :size 0.5)
          (navi-mode :select t :align left :size 0.1)
          (inferior-emacs-lisp-mode :select t :align below :size 0.2)
          ("*Org Select*" :select t :align below :size 0.33)
          ("*Org Note*" :select t :align below :size 0.33)
          ("*Org Links*" :select t :align below :size 0.2)
          ("*Org todo*" :select t :align below :size 0.2)
          ("\\`\\*e?shell" :select t :align below :size 0.3 :regexp t)
          ("*eshell.*" :select t :align below :size 0.5)
          ("*ielm*" :select t :align below :size 0.3)
          ("*Man.*" :select t :align below :size 0.5 :regexp t)
          ("*Org Src.*" :select t :align below :size 0.5 :regexp t))))

(use-package shell
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
             (buf (concat "*shell " (file-name-base default-directory) "*"))
             (proper-cwd (shell-quote-argument (expand-file-name cwd))))
        (shell buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (comint-kill-input)
          (insert (concat "cd " proper-cwd))
          (let ((comint-process-echoes t))
            (comint-send-input))
          (recenter 0))))))

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
    (add-hook 'sh-mode-hook
              (lambda ()
                (setq-local company-backends (append '(company-shell) company-backends)))))

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

(use-package slime
  :ensure t
  :commands (slime)
  :mode (("\\.cl\\'" . common-lisp-mode)
         ("\\.lisp\\'" . common-lisp-mode))
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")
  (setq slime-contribs '(slime-fancy)))

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
  (add-hook 'after-init-hook (lambda () (smartparens-global-mode)))
  (add-hook 'prog-mode-hook (lambda () (smartparens-strict-mode)))

  :config
  ;; Only use smartparens in web-mode
  (sp-local-pair 'text-mode " — " " — " :trigger "—" :wrap "C-—")
  (sp-local-pair 'text-mode "« " " »" :trigger "«" :wrap "C-«")
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

  (sp-local-pair 'c-mode  "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'cc-mode "{" nil :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'scheme-mode "<" ">")

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

(use-package smooth-scrolling :ensure t
  :config
  (smooth-scrolling-mode)
  (setq smooth-scroll-margin 5))

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t) ; make the fringe stand out from the background
  (setq solarized-use-variable-pitch t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-more-italic t)
  (setq solarized-scale-org-headlines t)
  (setq-default cursor-type 'bar)

  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

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
  (add-hook 'prog-mode-hook (lambda () (subword-mode 1))))

(use-package swiper :ensure t
  :bind* ("M-s" . swiper))

(use-package system-packages :ensure t
  :commands (system-packages-install
             system-packages-search))

;;;; T

(use-package tex-site
  :ensure auctex
  :commands (TeX-tex-mode)
  :mode (("\\.tex\\'" . TeX-tex-mode)
         ("\\.cls\\'" . TeX-tex-mode))
  :config
  (load-file "~/dotfile/emacs/latex-config.el"))

(use-package tiny :ensure t
  :bind* (("C-;" . tiny-expand)))

(use-package tramp
  :init
  (setq tramp-default-method "ssh"))

;;;; U

(use-package undo-tree
  :ensure t
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

  (bind-keys*
   ("C-x u" . sam|undo-tree-visualize)
   ("C-z" . sam|undo)
   ("C-S-z" . sam|redo))

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

  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local company-backends (append '(company-web-html) company-backends))
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
        which-key-side-window-max-height 0.3
        which-key-side-window-max-width 0.5
        which-key-idle-delay 0.3
        which-key-min-display-lines 7))

(use-package wrap-region :ensure t
  :diminish ""
  :config
  (wrap-region-global-mode -1)
  (wrap-region-add-wrappers
   '(("$" "$")
     (" " " ")
     ("{-" "-}" "#")
     ("/" "/" nil (ruby-mode org-mode))
     ("=" "=" nil (org-mode))
     ("/* " " */" "#" (java-mode javascript-mode css-mode))
     ("`" "`" nil (markdown-mode ruby-mode))
     ("*" "*" nil (markdown-mode org-mode))
     ("_" "_" nil (markdown-mode)))))

;;;; X

(use-package xterm-color
  :ensure t
  :config
  ;; comint install
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

;;;; Y

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


;;;; Z

;;; personal functions

(load-file "~/dotfile/emacs/functions.el")

;;; org

(load-file "~/dotfile/emacs/org.el")

;;; bind-key* "C-x x"  keybindings

(load-file "~/dotfile/emacs/keybindings.el")

;;; custom

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; TODO check out https://www.youtube.com/watch?v=m_Oj_6Bjryw
;; TODO check out https://github.com/Kungsgeten/selected.el
;; TODO check out https://vxlabs.com/2017/02/07/mu4e-0-9-18-e-mailing-with-emacs-now-even-better/
;; TODO check out https://github.com/atgreen/paperless
;; TODO check out https://github.com/masasam/emacs-easy-hugo
