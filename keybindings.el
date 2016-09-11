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
    "a" '(hydra-launcher/body :which-key "Applications")

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
    "é" '(hydra-window/body :which-key "Window")

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
    "T" '(hydra-toggle/body t :which-key "Toggle")

;;;; v
    ;; Git related stuff
    "v" '(hydra-git/body t :which-key "Version Control"))

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
   "M-b" 'ivy-switch-buffer
   "p" #'hydra-paste/evil-paste-after
   "P" #'hydra-paste/evil-paste-before  )

;;; INSERT map
  (imap
   "C-z" 'undo-tree-undo
   "C-|" 'ivy-switch-buffer
   "C-." 'hydra-move/body
   "C-é" 'hydra-window/body
   )


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
;;; OPERATOR map
  (general-omap
   :prefix "SPC"
    "." 'avy-goto-word-or-subword-1
    "l" 'evil-avy-goto-line
    "é" 'evil-avy-goto-subword-0 )

;;; MOTION map
  (mmap
   "t" 'evil-next-visual-line
   "s" 'evil-previous-visual-line
   )

;;; MODE specifique map
  (general-define-key :keymaps 'Buffer-menu-mode-map
    "." 'hydra-buffer-menu/body)

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

(defhydra hydra-toggle (:hint nil :color red)
  "
^themes^     ^modes^           ^modeline^          ^frame
^^^^^^^^--------------------------------------------------------
_d_: dark    _f_: flycheck     _t_: time           _F_: fullscreen
_l_: light   _n_: linum        ^ ^                 _m_: maximized
^ ^          _w_: whitespace   ^ ^                 ^ ^
"
  ("d" solarized-switch-to-dark)
  ("l" solarized-switch-to-light)
  ("f" flycheck-mode :color blue)
  ("n" nlinum-mode)
  ("t" display-time-mode)
  ("m" toggle-frame-maximized)
  ("F" toggle-frame-fullscreen :color blue)
  ("w" blank-mode)
  ("q" nil "quit" :color blue))

(defhydra hydra-buffer-menu
  (:color pink
   :hint nil)
  "
^Nav^      ^Mark^         ^Unmark^        ^Actions^          ^Search
^^^^^^^^----------------------------------------------------------
_p_: prev  _m_: mark      _u_: unmark     _x_: execute       _R_: re-isearch
_n_: next  _s_: save      _U_: unmark up  _b_: bury          _I_: isearch
^ ^        _d_: delete    ^ ^             _g_: refresh       _O_: multi-occur
^ ^        _D_: delete up ^ ^             _T_: files only: % -28`Buffer-menu-files-only
^ ^        _~_: modified
"
  ("p" previous-line)
  ("n" next-line)
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

(defhydra hydra-git
  (:body-pre (git-gutter-mode 1)
   :post (progn (kill-diff-buffers)
		(message "killed diff buffers"))
   :hint nil)
  "
^Nav^                 ^Hunk^            ^Files^        ^Actions^
^^^^^^^^----------------------------------------------------------
_n_: next hunk        _s_tage hunk      _S_tage        _c_ommit
_p_: previous hunk    _r_evert hunk     _R_evert       _b_lame
_P_: first hunk       _p_opup hunk      _d_iff         _C_heckout
_N_: last hunk        _R_evision start  _t_imemachine
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("P" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
  ("N" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("S" magit-stage)
  ("R" magit-revert)
  ("d" magit-diff-unstaged)
  ("t" git-timemachine)
  ("c" magit-commit)
  ("b" magit-blame)
  ("C" magit-checkout)
  ("v" magit-status "status" :color blue)
  ("q" nil "quit" :color blue)
  ("Q" (progn
	 (git-gutter-mode -1)
	 ;; git-gutter-fringe doesn't seem to
	 ;; clear the markup right away
	 (sit-for 0.1)
	 (git-gutter:clear))
   "quit git-gutter"
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
  ("q"   nil "cancel" :color blue))

(defhydra hydra-launcher (:color blue :hint nil)
  "
^Web^        ^Blog^       ^Explorer^       ^Apps^
^^^^^^^^--------------------------------------------------
_g_oogle    _bn_ew post  _d_ired          _s_hell
_r_eddit    _bp_ublish   _D_eer           _S_hell gotodir
_w_iki      _bs_server   _r_anger         _a_pps
_t_witter
"
  ("a" counsel-osx-apps)
  ("bn" (hugo-new-post))
  ("bp" (hugo-publish))
  ("bs" (hugo-server))
  ("d" dired)
  ("D" deer)
  ("r" ranger)
  ("g" (browse-url "https://www.google.fr/") )
  ("r" (browse-url "http://www.reddit.com/r/emacs/") )
  ("w" (browse-url "http://www.emacswiki.org/") )
  ("t" (browse-url "https://twitter.com/?lang=fr") )
  ("s" (sam--iterm-focus) )
  ("S" (sam--iterm-goto-filedir-or-home) )
  ("q" nil "quit"))

(defhydra hydra-paste (:color red :hint nil)
  "
   Paste : [%s(length kill-ring-yank-pointer)/%s(length kill-ring)]
         _n_ → cycle next          _p_ → paste before
         _N_ → cycle previous      _P_ → paste after
"
  ("N" evil-paste-pop-next)
  ("n" evil-paste-pop)
  ("p" evil-paste-after)
  ("P" evil-paste-before)
  ("u" undo "undo" :color red)
  ("l" counsel-yank-pop "list" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-move
  (:hint nil)
  "
 ^ ^ ^ ^ ^ ^ ^V^ ^ ^ ^ ^ ^ ^
 ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
 ^ ^ ^ ^ ^ ^ ^p^ ^ ^ ^ ^ ^ ^
 ^a^ ^ ^ ^b^ ^l^ ^f^ ^ ^ ^e^
 ^ ^ ^ ^ ^ ^ ^n^ ^ ^ ^ ^ ^ ^
 ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^ ^
 ^ ^ ^ ^ ^ ^ ^v^ ^ ^ ^ ^ ^ ^
"
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("v" scroll-up-command)
  ;; Converting M-v to V here by analogy.
  ("V" scroll-down-command)
  ("l" recenter-top-bottom)
  ("q" nil "quit" :color blue))


(defhydra hydra-window
  (:hint nil
   :color amaranth
   :columns 4
   )
  "
 ^Move^ ^^^^ ^ ^ ^ ^  ^Split^           ^ ^     ^Size^    ^ ^   ^Command^

 ^ ^ ^ ^ _S_ ^ ^ ^ ^   _it_: split H    ^ ^      ^ ^      ^ ^   _d_elete
 ^ ^ ^ ^ _s_ ^ ^ ^ ^   _-_ : split H    ^ ^      _p_: - H ^ ^   _m_aximize
 _C_ _c_ _a_ _r_ _R_   _|_ : split V    + W: _b_ ^=^ _f_: - W   _N_ew
 ^ ^ ^ ^ _t_ ^ ^ ^ ^   _ir_: split V    ^ ^      _n_: + H ^ ^
 ^ ^ ^ ^ _T_ ^ ^ ^ ^   _v_ : split V
"
  ("c" evil-window-left :color blue)
  ("r" evil-window-right :color blue)
  ("t" evil-window-bottom :color blue )
  ("s" evil-window-top :color blue)
  ("C" evil-window-move-far-left )
  ("R" evil-window-move-far-right )
  ("T" evil-window-move-very-bottom )
  ("S" evil-window-move-very-top )
  ;; splt
  ("it" evil-window-split )
  ("-" evil-window-split )
  ("|" evil-window-vsplit )
  ("ir" evil-window-vsplit )
  ("v" evil-window-vsplit :color blue)
  ;; delete other
  ("m" delete-other-windows )
  ("d" evil-window-delete )
  ;; change height and width
  ("f" evil-window-decrease-width )
  ("b" evil-window-increase-width )
  ("p" evil-window-decrease-height )
  ("n" evil-window-increase-height )

  ("N" evil-window-new :color blue)
  ("=" balance-windows )
  ("a" ace-window )
  ("q" nil "quit" :color blue))
