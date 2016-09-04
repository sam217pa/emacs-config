;; Most of the configuration should be done under general.el
;; Most of the keybindings should have a clear and precise prefix.
;; common functions should be under the same prefix (leader)
;;
;; Emacs related functions goes to the prefix `e'.
;; Major-mode related functions goes to the prefix `,'.


(use-package general :ensure t
  :config

  (general-evil-setup t)

  ;; This chunks contains all the keybindings that I use regularly.
  ;; They are placed under the prefix SPC, as in spacemacs.
  ;; Absolutely all functions that I use must be referenced here.
  ;; There is another leader via C-SPC which gives shorter keybindings
  ;; for stuff that I use more often.
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix " "

    ;; simple command
    "'"   '(sam--iterm-focus :which-key "iterm")
    "?"   '(sam--iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(sam--switch-to-other-buffer :which-key "prev buffer")
    "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired
    "ab" '(:ignore t :which-key "Blog")
    "abn" 'hugo-new-post
    "abs" 'hugo-server
    "ao"  'counsel-osx-app
    "ac"  '(sam--calendar-focus :which-key "calendar")
    "af"  '(sam--finder-focus :which-key "finder")
    "aF"  '(sam--finder-goto-filedir-or-home :which-key "finder - goto dir")

    ;; Buffer
    "b" '(:ignore t :which-key "Buffer")
    "bB"  '(ibuffer)
    "bb"  '(ivy-switch-buffer :which-key "switch buffer")
    "bd"  '(kill-buffer-and-window :which-key "delete buffer")
    "bn"  '(sam--new-empty-buffer :which-key "new empty buffer")

    ;; Comment or Compile
    "c" '(:ignore t :which-key "Comment")
    "cl"  '(sam--comment-or-uncomment-region-or-line :which-key "comment line")

    ;; Window management
    "é" '(:ignore t :which-key "Window")
    "éc"  'other-window
    "éd"  'ace-delete-window
    "ér"  'other-window
    "éé"  'hydra-window/body
    "éh"  '(split-window-vertically :which-key "split horizontal")
    "ém"  '(delete-other-windows :which-key "maximize current")
    "év"  '(split-window-horizontally :which-key "split vertical")
    "é|"  '(split-window-horizontally :which-key "split vertical")
    "é-"  '(split-window-vertically :which-key "split horizontal")

    ;; Find and Files
    "f" '(:ignore t :which-key "Files")
    "fd"  '(counsel-git :which-key "find in git dir")
    "fD"  '(sam--delete-current-buffer-file :which-key "delete file")
    "fe" '(:ignore t :which-key "edit")
    "fei" '(sam--edit-init-file :which-key "edit init")
    "fek" '(sam--edit-keybindings :which-key "edit keybindings")
    "fef" '(sam--edit-functions :which-key "edit functions")
    "fep" '(sam--edit-password :which-key "edit password")
    "ff"  '(find-file :which-key "find file")
    "fo"  '(sam--open-in-external-app :which-key "open file")
    "fr"  '(counsel-recentf :which-key "recent files")
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(rename-file :which-key "rename file")

    ;; Jump to :
    "g" '(:ignore t :which-key "Go to")
    "gc" 'avy-goto-char
    "gC" 'avy-goto-char-2
    "gl" 'avy-goto-line
    "gé" 'avy-goto-word-or-subword-1

    ;; Insert
    "i" '(:ignore t :which-key "Insert")
    "it"  '(sam--insert-timestamp :which-key "timestamp")
    "il" '(:ignore t :which-key "insert link")
    "ilm" '(sam--chrome-md-link :which-key "chrome - md")
    "ilo" '(sam--chrome-org-link :which-key "chrome - org")
    "ilf" '(sam--finder-md-link :which-key "finder - md")

    ;; Journal
    "j" '(:ignore t :which-key "Journal")

    ;; Lisp
    "l" '(:ignore t :which-key "Lisp")

    ;; Org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda-list

    ;; Quit
    "q" '(:ignore t :which-key "Quit")
    "qb" 'kill-buffer-if-not-modified
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
    "qr" '(restart-emacs :which-key "restart emacs")

    ;; Save and search
    "s" '(:ignore t :which-key "Save/Search")
    "s."  'save-buffer
    "s,"  'server-edit

    ;; text related
    "t" '(:ignore t :which-key "text")
    "ta" '(:ignore t :which-key "text align")
    "tar" 'align-region
    "taR" 'align-regexp
    "ti"  'indent-region
    "tr" '(vr/query-replace :which-key "text replace")

    ;; Toggle UI elements
    "T" '(:ignore t :which-key "Toggle")
    "TF" '(toggle-frame-fullscreen :which-key "fullscreen")
    "Tf" '(toggle-frame-maximized  :which-key "maximize")
    "Td" '(solarized-switch-to-dark :which-key "dark background")
    "Tl" '(solarized-switch-to-light :which-key "light background")
    "Tm" '(:ignore t :which-key "modeline")
    "Tmt" 'display-time-mode
    "Tn" '(linum-mode :which-key "line number")
    "Tw" 'blank-mode

    ;; Git related stuff
    "v" '(:ignore t :which-key "Version Control")
    "vb" 'magit-blame
    "vB" 'magit-blame-quit
    "vc" 'magit-commit
    "vC" 'magit-checkout
    "vd" 'magit-diff-unstaged
    "ve" 'magit-ediff-compare
    "vi" 'magit-init
    "vl" 'magit-log-current
    "vm" '(git-messenger:popup-message :which-key "git messenger")
    "vs" '(git-gutter:stage-hunk :which-key "stage hunk")
    "vS" 'magit-stage-file
    "vt" 'git-timemachine
    "vU" 'magit-unstage-file
    "vv" 'magit-status
    )

  ;; this is the second prefix. It gives shorter access to common
  ;; functions. Like avy goto line.
  (general-define-key
   :states '(normal insert emacs)
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC"
    "l" '(avy-goto-line)
    "a" 'align-regexp
    )


  ;; those are the direct keybindings. Just press the touch.
  (nmap
   "'" (general-simulate-keys "C-c")
   "é" 'evil-goto-mark
   "è" 'ace-window
   "s-b" 'ivy-switch-buffer
   "s-g" 'avy-goto-char
   "C-p" 'browse-kill-ring
   )

  (imap
   ;; restaure quelques commandes emacs par défault
   "C-a" 'beginning-of-line
   "C-d" 'delete-forward-char
   "C-e" 'end-of-line
   "C-f" 'forward-char
   "C-b" 'backward-char
   "C-n" 'evil-next-line
   "C-p" 'evil-previous-line
   )

  (mmap
   "t" 'evil-next-visual-line
   "s" 'evil-previous-visual-line
   )


  (general-define-key
   ;; SUPER map
   "s-l"   'sam--comment-or-uncomment-region-or-line
   "s-w"   'delete-other-windows
   "s-m"   'delete-other-windows
   "s-SPC" 'set-mark-command
   "s-TAB" 'sam--switch-to-other-buffer
   ;; HYPER map
   "H-F" 'toggle-frame-fullscreen
   "H-f" 'toggle-frame-maximized
   ;; META map
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   )
  )

;;
;; Which-key
;;

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
  "C-x v"   "version control")
;; key description for emacs-lisp state.
(which-key-add-key-based-replacements
  "SPC l%"   "evil-jump-item"
  "SPC l:"   "evil-ex"
  "SPC l("   "lisp-state-insert-sexp-before"
  "SPC l"    "lisp-state-insert-sexp-after"
  "SPC l$"   "sp-end-of-sexp"
  "SPC l`k"  "sp-kill-hybrid-sexp"
  "SPC l`p"  "sp-push-hybrid-sexp"
  "SPC l`s"  "sp-slurp-hybrid-sexp"
  "SPC l`t"  "sp-transpose-hybrid-sexp"
  "SPC l0"   "lisp-state-beginning-of-sexp"
  "SPC l1"   "digit-argument"
  "SPC l2"   "digit-argument"
  "SPC l3"   "digit-argument"
  "SPC l4"   "digit-argument"
  "SPC l5"   "digit-argument"
  "SPC l6"   "digit-argument"
  "SPC l7"   "digit-argument"
  "SPC l8"   "digit-argument"
  "SPC l9"   "digit-argument"
  "SPC la"   "sp-absorb-sexp"
  "SPC lb"   "sp-forward-barf-sexp"
  "SPC lB"   "sp-backward-barf-sexp"
  "SPC lc"   "sp-convolute-sexp"
  "SPC lds"  "sp-kill-symbol"
  "SPC lDs"  "sp-backward-kill-symbol"
  "SPC ldw"  "sp-kill-word"
  "SPC lDw"  "sp-backward-kill-word"
  "SPC ldx"  "sp-kill-sexp"
  "SPC lDx"  "sp-backward-kill-sexp"
  "SPC le"   "sp-splice-sexp-killing-forward"
  "SPC lE"   "sp-splice-sexp-killing-backward"
  "SPC lh"   "sp-backward-symbol"
  "SPC lH"   "sp-backward-sexp"
  "SPC li"   "evil-insert-state"
  "SPC lI"   "evil-insert-line"
  "SPC lj"   "lisp-state-next-closing-paren"
  "SPC lJ"   "sp-join-sexp"
  "SPC lk"   "lisp-state-prev-opening-paren"
  "SPC ll"   "lisp-state-forward-symbol"
  "SPC lL"   "sp-forward-sexp"
  "SPC lp"   "evil-paste-after"
  "SPC lP"   "evil-paste-before"
  "SPC lr"   "sp-raise-sexp"
  "SPC ls"   "sp-forward-slurp-sexp"
  "SPC lS"   "sp-backward-slurp-sexp"
  "SPC lt"   "sp-transpose-sexp"
  "SPC lu"   "undo-tree-undo"
  "SPC lU"   "sp-backward-up-sexp"
  "SPC lC-r" "undo-tree-redo"
  "SPC lv"   "evil-visual-char"
  "SPC lV"   "evil-visual-line"
  "SPC lC-v" "evil-visual-block"
  "SPC lw"   "lisp-state-wrap"
  "SPC lW"   "sp-unwrap-sexp"
  "SPC ly"   "sp-copy-sexp"
  )


;;
;; Hydra
;;
