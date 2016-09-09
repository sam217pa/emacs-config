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
;;; SPC-map
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix " "

    ;; simple command
    "'"   '(sam--iterm-focus :which-key "iterm")
    "?"   '(sam--iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(sam--switch-to-other-buffer :which-key "prev buffer")
    "." '(avy-goto-word-or-subword-1  :which-key "go to char")
    "SPC" '(counsel-M-x  :which-key "M-x")

;;;; a
    ;; Applications
    "a" '(:ignore t :which-key "Applications")
    "ar" 'ranger
    "ad" 'dired
    "ab" '(:ignore t :which-key "Blog")
    "abn" 'hugo-new-post
    "abs" 'hugo-server
    "abp" 'hugo-publish
    "ao"  'counsel-osx-app
    "ac"  '(sam--calendar-focus :which-key "calendar")
    "af"  '(sam--finder-focus :which-key "finder")
    "aF"  '(sam--finder-goto-filedir-or-home :which-key "finder - goto dir")

;;;; b
    ;; Buffer
    "b" '(:ignore t :which-key "Buffer")
    "bB"  '(ibuffer)
    "bb"  '(ivy-switch-buffer :which-key "switch buffer")
    "bd"  '(kill-buffer-and-window :which-key "delete buffer")
    "bn"  '(sam--new-empty-buffer :which-key "new empty buffer")

;;;; c
    ;; Comment or Compile
    "c" '(:ignore t :which-key "Comment")
    "cl"  '(sam--comment-or-uncomment-region-or-line :which-key "comment line")

;;;; é
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

;;;; f
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
    "fR"  '(fasd-find-file)
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(rename-file :which-key "rename file")

;;;; g
    ;; Jump to :
    "g" '(:ignore t :which-key "Go to")
    "gc" 'avy-goto-char
    "gC" 'avy-goto-char-2
    "gl" 'avy-goto-line
    "gé" 'avy-goto-word-or-subword-1

;;;; i
    ;; Insert
    "i" '(:ignore t :which-key "Insert")
    "it"  '(sam--insert-timestamp :which-key "timestamp")
    "il" '(:ignore t :which-key "insert link")
    "ilm" '(sam--chrome-md-link :which-key "chrome - md")
    "ilo" '(sam--chrome-org-link :which-key "chrome - org")
    "ilf" '(sam--finder-md-link :which-key "finder - md")
    "iL" '(:ignore t :which-key "lorem")
    "iLs" 'lorem-ipsum-insert-sentences
    "iLp" 'lorem-ipsum-insert-paragraphs
    "iLl" 'lorem-ipsum-insert-list

;;;; j
    ;; Journal
    "j" '(:ignore t :which-key "Journal")

;;;; l
    ;; Lisp
    "l" '(:ignore t :which-key "Lisp")

;;;; o
    ;; Org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda-list

;;;; p
    ;; Project
    "p" '(hydra-projectile/body :which-key "Project")
    ;; "pf" 'counsel-git
;;;; q
    ;; Quit
    "q" '(:ignore t :which-key "Quit")
    "qb" 'kill-buffer-if-not-modified
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
    "qr" '(restart-emacs :which-key "restart emacs")

;;;; s
    ;; Save and search
    "s" '(:ignore t :which-key "Save/Search")
    "s."  'save-buffer
    "s,"  'server-edit

;;;; t
    ;; text related
    "t" '(:ignore t :which-key "text")
    "ta" '(:ignore t :which-key "text align")
    "tar" 'align-region
    "taR" 'align-regexp
    "ti"  'indent-region
    "tr" '(vr/query-replace :which-key "text replace")

;;;; T
    ;; Toggle UI elements
    "T" '(:ignore t :which-key "Toggle")
    "TF" '(toggle-frame-fullscreen :which-key "fullscreen")
    "Tf" '(toggle-frame-maximized  :which-key "maximize")
    "Td" '(solarized-switch-to-dark :which-key "dark background")
    "Tl" '(solarized-switch-to-light :which-key "light background")
    "Tm" '(:ignore t :which-key "modeline")
    "Tmt" 'display-time-mode
    "Tn" '(nlinum-mode :which-key "line number")
    "Tw" 'blank-mode

;;;; v
    ;; Git related stuff
    "v" '(:ignore t :which-key "Version Control")
    "vb" 'magit-blame
    "vB" 'magit-blame-quit
    "vc" 'magit-commit
    "vC" 'magit-checkout
    "vd" 'magit-diff-unstaged
    "ve" 'magit-ediff-compare
    "vf" 'git-gutter:next-hunk
    "vi" 'magit-init
    "vl" 'magit-log-current
    "vm" '(git-messenger:popup-message :which-key "git messenger")
    "vp" 'git-gutter:previous-hunk
    "vr" 'git-gutter:revert-hunk
    "vR" 'magit-revert
    "vs" '(git-gutter:stage-hunk :which-key "stage hunk")
    "vS" 'magit-stage-file
    "vt" 'git-timemachine
    "vU" 'magit-unstage-file
    "vv" 'magit-status)

  ;; this is the second prefix. It gives shorter access to common
  ;; functions. Like avy goto line.
;;; C-SPC map
  (general-define-key
   :states '(normal insert emacs)
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC"
    "l" '(avy-goto-line)
    "a" 'align-regexp
    )


  ;; those are the direct keybindings. Just press the touch.
;;; NORMAL map
  (nvmap
   "'" (general-simulate-keys "C-c")
   "é" 'evil-goto-mark
   "è" 'ace-window
   "s-b" 'ivy-switch-buffer
   "s-g" 'avy-goto-char
   "C-p" 'counsel-yank-pop
   "|" 'ivy-switch-buffer
   "c" 'evil-backward-char
   "C" 'evil-window-top
   "t" 'evil-next-line
   "s" 'evil-previous-line
   "r" 'evil-forward-char
   "R" 'evil-window-bottom
   "j" 'evil-avy-goto-char-in-line
   "J" 'evil-find-char-to-backward
   "h" 'evil-change
   "H" 'evil-change-line
   "T" 'evil-join
   "l" 'evil-replace
   "L" 'evil-replace-state
   "k" 'evil-substitute
   "K" 'evil-change-whole-line
   )

;;; INSERT map
  (imap
   "C-z" 'undo-tree-undo
   "C-|" 'ivy-switch-buffer
   )

;;; OPERATOR map
  (general-omap
   :prefix "SPC"
    "." 'avy-goto-word-or-subword-1
    "l" 'evil-avy-goto-line
    "é" 'evil-avy-goto-subword-0 )

  ;; this one is genius from general. you press ".", it wait for another command
  ;; in the general-key-dispatch list of command or insert .
  ;; really useful in insert map, no need to go to escape map.
  (general-imap ","
		(general-key-dispatch 'self-insert-command
		  "b" 'ivy-switch-buffer
		  "c" 'avy-goto-word-1
		  "l" 'avy-goto-line
		  "s" 'save-buffer
		  "p" 'projectile-command-map))

;;; MOTION map
  (mmap
   "t" 'evil-next-visual-line
   "s" 'evil-previous-visual-line
   )


  (general-define-key
;;; SUPER map
   "s-l"   'sam--comment-or-uncomment-region-or-line
   "s-w"   'delete-other-windows
   "s-m"   'delete-other-windows
   "s-SPC" 'set-mark-command
   "s-<tab>" 'sam--switch-to-other-buffer
;;; HYPER map
   "H-F" 'toggle-frame-fullscreen
   "H-f" 'toggle-frame-maximized
   "H-b" 'ivy-switch-buffer
   "H-r" 'counsel-recentf
;;; META map
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   ))

;;
;;; Which-key
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


;;
;; Hydra
;;

(defhydra hydra-toggle (:hint nil)
  "
^UI^
^^^^^^^^----
_n_: linenumber
_f_: fullscreen
_q_: quit
"
  ("n" nlinum-mode)
  ("f" toggle-frame-fullscreen)
  ("q" nil "quit")
  )



(defhydra hydra-buffer-menu
  (:color pink
   :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-git-gutter
  (:body-pre (git-gutter-mode 1) :hint nil)
  "
Git gutter:
  _n_: next hunk        _s_tage hunk     _q_uit
  _p_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _P_: first hunk
  _N_: last hunk        set start _R_evision
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("P" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("N" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
   :color blue))

(defhydra hydra-projectile
  (:color teal :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))
