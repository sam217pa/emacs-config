;; Most of the configuration should be done under general.el
;; Most of the keybindings should have a clear and precise prefix.
;; common functions should be under the same prefix (leader)
;;

;; I define most of my frequently used keybindings under the SPC
;; prefix. Free keyboard touch are ê, à, ç, é, è.
;;
;; The META map can be used in different ways between the normal map
;; and the insert map. In insert state, in should respect emacs
;; default, since it is often used by packages. It is free in normal
;; state map.
;;
;; The SUPER and HYPER key are kind of hard to use, but they can be
;; used as placeholder for common functions. In ess, one can give
;; different function to M-RET, C-RET, S-RET or H-RET for example. It
;; should nonetheless stay consistent and obvious what each keypress
;; does. When it's not, consider wrapping it with which-key or an hydra.
;;
;; I tend to attribute the same meaning to S-x and H-x, but the H-x
;; must feel stronger than the S-x. Like maximise with S-x and
;; fullscreen with H-x. It is not really consistent for now, as the
;; S-x is sometimes more useful than the H-x definition.
;;
;; I also and finally use key-seq, a key-chord related packages, but
;; the order of keys matters. I constructed a prefix around x, a touch
;; that I do not press that often in french. Like xs to save the
;; buffer, without leaving the insert mode. Or xv to stage the current
;; hunk. They key-seq keymap must be strictly reserved to really
;; frequent functions. The xs sequence for example allow me to save
;; the buffer with two keypress instead of having to press `C-x C-s`,
;; or `xq (ESC) SPC s .`. It must not be bloated. I reserve it for
;; function that I use often in insert state.

(use-package general :ensure t
  :config

  (general-evil-setup t)


;;; SPC-map
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix " "

    ;; simple command
    "'"   '(sam--iterm-focus :which-key "iterm")
    "?"   '(sam--iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(ivy-switch-buffer :which-key "prev buffer")
    "." '(avy-goto-word-or-subword-1  :which-key "go to char")
    "SPC" '(counsel-M-x)

    ;; Applications
    "a" '(hydra-launcher/body :which-key "Applications")
    ;; buffer
    "b" '(hydra-buffer/body t :which-key "Buffer")
    ;; Comment or Compile
    "c" '(:ignore t :which-key "Comment")
    "cl"  '(sam--comment-or-uncomment-region-or-line :which-key "comment line")
    ;; Window management
    "é" '(hydra-window/body :which-key "Window")
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
    "fF"  '(find-file-other-window :which-key "ff other  window")
    "fo"  '(sam--open-in-external-app :which-key "open file")
    "fr"  '(ivy-switch-buffer :which-key "recent files")
    "fR"  '(fasd-find-file)
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(rename-file :which-key "rename file")
    "ft"  '(sam--edit-todo)
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
    "iL" '(:ignore t :which-key "lorem")
    "iLs" 'lorem-ipsum-insert-sentences
    "iLp" 'lorem-ipsum-insert-paragraphs
    "iLl" 'lorem-ipsum-insert-list
    ;; Journal
    "j" '(hydra-journal/body t :which-key "Journal")
    ;; Org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda-list
    ;; Project
    "p" '(hydra-projectile/body :which-key "Project")
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
    "tar" 'align-regexp
    "ti"  'indent-region
    "tr" '(vr/query-replace :which-key "text replace")
    ;; Toggle UI elements
    "T" '(hydra-toggle/body :which-key "Toggle")
    ;; zoom
    "z" '(hydra-zoom/body :which-key "zoom")
    ;; Git related stuff
    "v" '(hydra-git/body :which-key "Version Control"))

  ;; those are the direct keybindings. Just press the touch.
;;; NORMAL map
  (nvmap
   "'" (general-simulate-keys "C-c")
   "é" 'evil-goto-mark
   "è" 'ace-window
   "s-b" 'ivy-switch-buffer
   "s-g" 'avy-goto-char
   "C-p" 'counsel-yank-pop
   "C-'" 'eshell-here
   "?" 'avy-goto-char-in-line
   "f" 'avy-goto-char-in-line
   "|" 'ivy-switch-buffer
   "c" 'evil-backward-char
   "C" 'evil-window-top
   "t" 'evil-next-line
   "s" 'evil-previous-line
   "r" 'evil-forward-char
   "R" 'evil-window-bottom
   "j" 'evil-avy-goto-char-in-line
   "J" 'evil-find-char-to-backward
   "l" 'evil-change
   "L" 'evil-change-line
   (general-chord "ll") 'avy-goto-line
   "T" 'evil-join
   "h" 'evil-replace
   "H" 'evil-replace-state
   "k" 'evil-substitute
   "K" 'evil-change-whole-line
   "M-b" 'ivy-switch-buffer
   "p" #'hydra-paste/evil-paste-after
   "P" #'hydra-paste/evil-paste-before
   (general-chord "xx") 'avy-goto-word-or-subword-1)

;;; INSERT map
  (iemap
   "C-z" 'undo-tree-undo
   "C-|" 'ivy-switch-buffer
   "C-." 'hydra-move/body
   "C-é" 'hydra-window/body
   "C-è" 'ace-window)



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

;;; MODE specific map
  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  (define-key emacs-lisp-mode-map (kbd "s-e") 'eval-defun)

  (general-define-key
;;; C-x MAP
   "C-x C-b" 'ibuffer
;;; SUPER map (alt left)
   "s-l"   'sam--comment-or-uncomment-region-or-line
   "s-w"   'delete-other-windows
   "s-m"   'delete-other-windows
   "s-d"   'kill-buffer-and-window
   "s-SPC" 'set-mark-command
   "s-<tab>" 'sam--switch-to-other-buffer
   "s-f" 'projectile-find-file
   "s-t" 'move-text-down
   "s-s" 'move-text-up
;;; HYPER map (ctlr left)
   "H-F" 'toggle-frame-fullscreen
   "H-f" 'toggle-frame-maximized
   "H-b" 'ivy-switch-buffer
   "H-r" 'counsel-recentf
   "H-m" 'delete-other-frames
;;; META map (cmd right)
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-g" 'hydra-error/body))
(use-package general :ensure t
  :config

  (general-evil-setup t)


;;; SPC-map
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix " "

    ;; simple command
    "'"   '(sam--iterm-focus :which-key "iterm")
    "?"   '(sam--iterm-goto-filedir-or-home :which-key "iterm - goto dir")
    "/"   'counsel-ag
    "TAB" '(ivy-switch-buffer :which-key "prev buffer")
    "." '(avy-goto-word-or-subword-1  :which-key "go to char")
    "SPC" '(counsel-M-x)

    ;; Applications
    "a" '(hydra-launcher/body :which-key "Applications")
    ;; buffer
    "b" '(hydra-buffer/body t :which-key "Buffer")
    ;; Comment or Compile
    "c" '(:ignore t :which-key "Comment")
    "cl"  '(sam--comment-or-uncomment-region-or-line :which-key "comment line")
    ;; Window management
    "é" '(hydra-window/body :which-key "Window")
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
    "fF"  '(find-file-other-window :which-key "ff other  window")
    "fo"  '(sam--open-in-external-app :which-key "open file")
    "fr"  '(ivy-switch-buffer :which-key "recent files")
    "fR"  '(fasd-find-file)
    "fs"  '(save-buffer :which-key "save file")
    "fS"  '(rename-file :which-key "rename file")
    "ft"  '(sam--edit-todo)
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
    "iL" '(:ignore t :which-key "lorem")
    "iLs" 'lorem-ipsum-insert-sentences
    "iLp" 'lorem-ipsum-insert-paragraphs
    "iLl" 'lorem-ipsum-insert-list
    ;; Journal
    "j" '(hydra-journal/body t :which-key "Journal")
    ;; Org
    "o" '(:ignore t :which-key "Org")
    "oa" 'org-agenda-list
    ;; Project
    "p" '(hydra-projectile/body :which-key "Project")
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
    "tar" 'align-regexp
    "ti"  'indent-region
    "tr" '(vr/query-replace :which-key "text replace")
    ;; Toggle UI elements
    "T" '(hydra-toggle/body :which-key "Toggle")
    ;; zoom
    "z" '(hydra-zoom/body :which-key "zoom")
    ;; Git related stuff
    "v" '(hydra-git/body :which-key "Version Control"))

  ;; those are the direct keybindings. Just press the touch.
;;; NORMAL map
  (nvmap
   "'" (general-simulate-keys "C-c")
   "é" 'evil-goto-mark
   "è" 'ace-window
   "s-b" 'ivy-switch-buffer
   "s-g" 'avy-goto-char
   "C-p" 'counsel-yank-pop
   "C-'" 'eshell-here
   "?" 'avy-goto-char-in-line
   "f" 'avy-goto-char-in-line
   "|" 'ivy-switch-buffer
   "c" 'evil-backward-char
   "C" 'evil-window-top
   "t" 'evil-next-line
   "s" 'evil-previous-line
   "r" 'evil-forward-char
   "R" 'evil-window-bottom
   "j" 'evil-avy-goto-char-in-line
   "J" 'evil-find-char-to-backward
   "l" 'evil-change
   "L" 'evil-change-line
   "T" 'evil-join
   "h" 'evil-replace
   "H" 'evil-replace-state
   "k" 'evil-substitute
   "K" 'evil-change-whole-line
   "M-b" 'ivy-switch-buffer
   "p" #'hydra-paste/evil-paste-after
   "P" #'hydra-paste/evil-paste-before)

;;; INSERT map
  (iemap
   "C-z" 'undo-tree-undo
   "C-|" 'ivy-switch-buffer
   "C-." 'hydra-move/body
   "C-é" 'hydra-window/body
   "C-è" 'ace-window)



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

;;; MODE specific map
  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  (define-key emacs-lisp-mode-map (kbd "s-e") 'eval-defun)

  (general-define-key
;;; C-x MAP
   "C-x C-b" 'ibuffer
;;; SUPER map (alt left)
   "s-l"   'sam--comment-or-uncomment-region-or-line
   "s-w"   'delete-other-windows
   "s-m"   'delete-other-windows
   "s-d"   'kill-buffer-and-window
   "s-SPC" 'set-mark-command
   "s-<tab>" 'sam--switch-to-other-buffer
   "s-f" 'projectile-find-file
   "s-t" 'move-text-down
   "s-s" 'move-text-up
;;; HYPER map (ctlr left)
   "H-F" 'toggle-frame-fullscreen
   "H-f" 'toggle-frame-maximized
   "H-b" 'ivy-switch-buffer
   "H-r" 'counsel-recentf
   "H-m" 'delete-other-frames
;;; META map (cmd right)
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-g" 'hydra-error/body))

;;
;;; ======================================================================
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
  "C-x v"   "version control"
  "C-c &"   "yas"
  "C-c @"   "hide-show"
  )

;;
;;; Key-chord
;;

(use-package key-chord :ensure t
  :defer 1
  :config
  (setq key-chord-two-keys-delay 0.2)
  ;; need to use key-seq. otherwise key order does not matter. that's bad.
  ;; i want latency only on x. not on everytouch.
  (use-package key-seq :ensure t
    :config
    ;; insert q prefix
    (key-seq-define evil-insert-state-map "qb" #'hydra-buffer/ivy-switch-buffer-and-exit)
    (key-seq-define evil-insert-state-map "qd" #'kill-this-buffer)
    (key-seq-define evil-insert-state-map "qf" #'ivy-switch-buffer)
    (key-seq-define evil-insert-state-map "ql" #'avy-goto-line)
    (key-seq-define evil-insert-state-map "qs" #'save-buffer)
    (key-seq-define evil-insert-state-map "qp" #'hydra-projectile/body)
    (key-seq-define evil-insert-state-map "QV" #'magit-status)
    (key-seq-define evil-insert-state-map "qq" #'fill-paragraph)
    (key-seq-define evil-insert-state-map "qQ" #'unfill-paragraph)
    ;; x prefix
    (key-seq-define evil-insert-state-map "xv" #'git-gutter:stage-hunk)
    (key-seq-define evil-insert-state-map "xc" #'avy-goto-word-1)
    ;; normal
    ;; q
    (key-seq-define evil-normal-state-map "qd" #'kill-this-buffer)
    (key-seq-define evil-normal-state-map "qf" #'counsel-find-file)
    (key-seq-define evil-normal-state-map "ql" #'avy-goto-line)
    (key-seq-define evil-normal-state-map "qs" #'save-buffer)
    ;; normal x
    (key-seq-define evil-normal-state-map "xc" #'avy-goto-word-1)))

;;
;;; Global
;;

;; from http://kitchingroup.cheme.cmu.edu/blog/2014/08/31/Using-Mac-gestures-in-Emacs/
(when (eq system-type 'darwin)
  (defvar *my-previous-buffer* t
    "can we switch?")

  (defun my-previous-buffer ()
    (interactive)
    (message "custom prev: *my-previous-buffer*=%s" *my-previous-buffer*)
    (when *my-previous-buffer*
      (previous-buffer)
      (setq *my-previous-buffer* nil)
      (run-at-time "1 sec" nil (lambda ()
                                 (setq *my-previous-buffer* t)))))

  (defvar *my-next-buffer* t
    "can we switch?")

  (defun my-next-buffer ()
    (interactive)
    (message "custom prev: *my-next-buffer*=%s" *my-next-buffer*)
    (when *my-next-buffer*
      (next-buffer)
      (setq *my-next-buffer* nil)
      (run-at-time "1 sec" nil (lambda ()
                                 (setq *my-next-buffer* t)))))

  (global-set-key [double-wheel-right] 'my-previous-buffer)
  (global-set-key [double-wheel-left] 'my-next-buffer))

;;; ======================================================================
;;; Hydra
;;

(defhydra hydra-toggle (:hint nil :color blue)
  "
^themes^     ^modes^           ^modeline^          ^frame
^^^^^^^^--------------------------------------------------------
_d_: dark    _f_: flycheck     _T_: time           _F_: fullscreen
_l_: light   _n_: linum        ^ ^                 _m_: maximized
^ ^          _w_: whitespace   ^ ^                 ^ ^
^ ^          _p_: persp-mode   ^ ^                 ^ ^
"
  ("d" solarized-switch-to-dark)
  ("l" solarized-switch-to-light)
  ("f" flycheck-mode :color blue)
  ("n" nlinum-mode)
  ("T" display-time-mode)
  ("p" persp-mode)
  ("m" toggle-frame-maximized)
  ("F" toggle-frame-fullscreen :color blue)
  ("w" blank-mode :color red)
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
_C-P_: first hunk     _P_opup hunk      _d_iff         _C_heckout
_C-N_: last hunk      _R_evision start  _t_imemachine
"
  ("n" git-gutter:next-hunk)
  ("p" git-gutter:previous-hunk)
  ("C-P" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)))
  ("C-N" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("P" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("S" magit-stage-file)
  ("R" magit-revert)
  ("d" magit-diff-unstaged :color blue)
  ("t" git-timemachine :color blue)
  ("c" magit-commit :color blue)
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

  ^Find File^        ^Search/Tags^        ^Buffers^       ^Cache^                    ^Project^
  ^---------^        ^-----------^        ^-------^       ^-----^                    ^-------^
  _f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
  _F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
  _C-f_: file pwd    _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
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
  ("R" (browse-url "http://www.reddit.com/r/emacs/"))
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
   :columns 4)
  "
 ^Move^ ^^^^ ^ ^ ^ ^  ^Split^           ^ ^     ^Size^    ^ ^   ^Command^   ^Window^

 ^ ^ ^ ^ _S_ ^ ^ ^ ^   _it_: split H    ^ ^      ^ ^      ^ ^   _d_elete    ^1^ ^2^ ^3^ ^4^
 ^ ^ ^ ^ _s_ ^ ^ ^ ^   _-_ : split H    ^ ^      _p_: - H ^ ^   _m_aximize  ^5^ ^6^ ^7^ ^8^
 _C_ _c_ _a_ _r_ _R_   _|_ : split V    + W: _b_ ^=^ _f_: - W   _N_ew       ^9^ ^0^
 ^ ^ ^ ^ _t_ ^ ^ ^ ^   _ir_: split V    ^ ^      _n_: + H ^ ^
 ^ ^ ^ ^ _T_ ^ ^ ^ ^   _v_ : split V
"
  ("c" evil-window-left :color blue)
  ("r" evil-window-right :color blue)
  ("t" evil-window-down :color blue )
  ("s" evil-window-up :color blue)
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

  ("0" select-window-0 :color blue)
  ("1" select-window-1 :color blue)
  ("2" select-window-2 :color blue)
  ("3" select-window-3 :color blue)
  ("4" select-window-4 :color blue)
  ("5" select-window-5 :color blue)
  ("6" select-window-6 :color blue)
  ("7" select-window-7 :color blue)
  ("8" select-window-8 :color blue)
  ("9" select-window-9 :color blue)

  ("N" evil-window-new :color blue)
  ("=" balance-windows )
  ("a" ace-window )
  ("." hydra-buffer/body "buffers" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-error ()
  ("t" next-error "next")
  ("n" next-error "next")
  ("s" previous-error "previous")
  ("p" previous-error "previous"))

(defhydra hydra-buffer (:color blue :columns 3)
  "
                Buffers :
  "
  ("n" next-buffer "next" :color red)
  ("b" ivy-switch-buffer "switch")
  ("B" ibuffer "ibuffer")
  ("p" previous-buffer "prev" :color red)
  ("C-b" buffer-menu "buffer menu")
  ("N" evil-buffer-new "new")
  ("d" kill-this-buffer "delete" :color red)
  ("ð" (progn (kill-this-buffer) (delete-window)) "del + wind" :color red)
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
  ("s" save-buffer "save" :color red)
  ("." hydra-window/body "window" :color blue))


;; Ibuffer
;; this is genius hydra making from
;; https://github.com/abo-abo/hydra/wiki/Ibuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _s_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
  _r_:  visit | _u_: unmark   | _S_: save        | _O_: sort
  _t_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("t" ibuffer-forward-line)
  ("r" ibuffer-visit-buffer :color blue)
  ("s" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("O" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" ibuffer-quit "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-dired-main (:color pink :hint nil :columns 4)
  "
^^^Nav^ ^^   ^Edit^                ^Mark^      ^Action^
^^^---^ ^^   ^----^                ^----^      ^------^
^ ^ _s_ ^ ^  _o_pen other window   _m_ark      _h_: show hidden
_c_ ^ ^ _r_  _R_ename              _u_nmark    _q_uit
^ ^ _t_ ^ ^  _S_ort                _d_elete
"
  ("t" dired-next-line :color red)
  ("s" dired-previous-line :color red)
  ("r" dired-find-file :color red)
  ("c" dired-up-directory :color red)
  ("o" dired-find-file-other-window :color blue)
  ("R" dired-rename-file )
  ("S" dired-sort-toggle-or-edit )
  ("u" dired-unmark )
  ("m" dired-mark )
  ("d" hydra-dired-delete/body :color blue)
  ("h" dired-omit-mode )
  ("q" nil "quit" :color blue))

(defhydra hydra-dired-delete (:color pink :hint nil :columns 4)
  ("d" dired-flag-file-deletion "flag delete")
  ("x" dired-do-flagged-delete "DEL flagged")
  ("D" dired-do-delete "delete this")
  ("q" hydra-dired-main/body "back" :color blue))

(defhydra hydra-journal (:color pink :hint nil :columns 4)
  "
Journal
"
  ("n" org-journal-new-entry "new" :color blue)
  ("/" org-journal-search-forever "search" :color blue)
  ("c" hydra-calendar/body "calendar" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-calendar
  (:color pink
   :hint nil
   :columns 4
   :body-pre (calendar)
   )
  "
 ^Calendar^     ^Journal^   ^ ^           ^Quit^
  _._: today    _n_ext      _e_: edit     _b_: journal
  _?_: date     _p_revious  _C-e_: view   _q_: quit
  ^ ^           _N_ew
"
  ("." calendar-goto-today)
  ("?" calendar-goto-date)
  ("n" org-journal-next-entry)
  ("p" org-journal-previous-entry)
  ("e" org-journal-read-entry :color blue)
  ("C-e" org-journal-display-entry)
  ("N" org-journal-new-date-entry)
  ("b" hydra-journal/body :color blue :exit-function (calendar-exit))
  ("q" nil "quit" :color blue :exit-function (calendar-exit)))

(defhydra hydra-zoom (:hint nil)
  "
^Zoom Buffer^  ^Zoom Frame^   ^Action^
_t_: in        _T_: in        _0_: reset
_s_: out       _S_: out       _q_: quit
"
  ("t" zoom-in )
  ("s" zoom-out )
  ("T" zoom-frm-in )
  ("S" zoom-frm-out )
  ("0" zoom-frm-unzoom)
  ("q" nil :color blue))

;;; ======================================================================
