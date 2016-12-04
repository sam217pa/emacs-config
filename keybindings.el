(use-package general :ensure t
  :config

;;; C-
  (general-define-key
   "C-S-c" 'sp-splice-sexp
   "C-S-z" 'undo-tree-redo
   "C-S-s" 'counsel-ag
   "C-S-k" 'kill-whole-line

   "C-r" 'sp-slurp-hybrid-sexp
   "C- " 'mark-line
   "C-é" 'hydra-window/body
   "C-'" 'avy-goto-word-or-subword-1
   "C-." 'hydra-main/body)

;;; C-M-
  (general-define-key
   "C-M-i" 'complete-symbol)

;;; C-c
  (general-define-key
   "C-c v" 'magit-status)

;;; C-x
  (general-define-key
   "C-x SPC" 'hydra-rectangle/body
   "C-x d" 'dired-other-window
   "C-x l" 'sam--chrome-plain-link
   "C-x n" 'narrow-or-widen-dwim
   "C-x p" 'hydra-projectile/body
   "C-x o" 'other-window
   "C-x v" 'hydra-git/body
   "C-x =" 'balance-windows

   "C-x C-b" 'ibuffer
   "C-x C-r" 'ivy-switch-buffer
   "C-x M-b" 'hydra-frame/body)

;;; M-
  (general-define-key
   "M-<backspace>" 'delete-to-sentence-beg
   "M-é" 'ace-window
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-ê" 'hydra-error/body
   "M-g" 'avy-goto-char-in-line

   "M-s-n" 'forward-paragraph
   "M-s-p" 'backward-paragraph)

;;; s-
  (general-define-key
   "s-<backspace>" 'ivy-switch-buffer
   "s-<tab>" 'sam--switch-to-other-buffer
   "s-c" 'windmove-left
   "s-r" 'windmove-right
   "s-d" 'kill-buffer-and-window
   "s-f" 'projectile-find-file
   "s-j" (lambda () (interactive) (join-line 4))
   "s-k" (lambda () (interactive)
           (save-excursion
             (move-beginning-of-line nil)
             (kill-visual-line -1)))    ; delete previous line
   "s-l" 'sam--comment-or-uncomment-region-or-line
   "s-q" nil                        ; don't close emacs with option q.
   "s-u" 'negative-argument
   "s-w" 'delete-other-windows
   "s-W" 'delete-window
   "s-'" 'avy-goto-char-2
   "s-." 'hydra-secondary/body
   "s-\"" 'ffap
   "s-(" 'hydra-sp/body)

;;; H-
  (general-define-key
   "H-<backspace>" 'ivy-switch-buffer-other-window
   "H-<tab>" 'hydra-outline/body
   "H-'" 'sam--iterm-goto-filedir-or-home
   "H-F" 'toggle-frame-maximized
   "H-b" 'ivy-switch-buffer
   "H-f" 'toggle-frame-fullscreen
   "H-l" 'sam--duplicate-line
   "H-m" 'delete-other-frames
   "H-n" 'buffer-to-new-frame
   "H-s" 'move-text-up
   "H-t" 'move-text-down
   "H-r" 'counsel-recentf
   "H-u" 'revert-buffer
   "H-w" 'ace-delete-window

   ;; H-M-
   "H-M-p" 'scroll-up-command
   "H-M-n" 'scroll-down-command)

;;; Key chords
  (use-package key-chord :ensure t
    :defer 1
    :config
    (setq key-chord-two-keys-delay 0.2))

  (general-define-key
   (general-chord "qq") #'avy-goto-word-or-subword-1
   (general-chord "qb") #'ivy-switch-buffer
   (general-chord "qd") #'kill-this-buffer
   (general-chord "qf") #'delete-frame
   (general-chord "ql") (lambda () (interactive) (avy-goto-line 4))
   (general-chord "qs") #'save-buffer
   (general-chord "qw") #'delete-window
   (general-chord "VV") #'magit-status)

;;; Mode specific map
  (general-define-key :keymaps 'Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  (general-define-key :keymaps 'emacs-lisp-mode-map "s-e" 'eval-defun)
  (general-define-key :keymaps 'shell-mode-map "H-c" 'erase-buffer)
  (general-define-key :keymaps 'term-mode-map "H-c" 'erase-buffer))


;;; Which-key

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
  "C-c @"   "hide-show")

;;; Global

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

;;; Hydra

(defhydra hydra-buffer (:color blue :columns 3 :hint nil)
  "
^NAV^     ^MENU^           ^DELETE^           ^ACTIONS^
_n_ext      _b_: switch      _d_: del ←         ^ ^
_p_rev    _M-b_: ibuffer   _C-d_: del →       _s_ave
^ ^       _C-b_: menu      _M-d_: del + win   _._: window
  "
  ("n" next-buffer :color red)
  ("p" previous-buffer :color red)
  ("b" ivy-switch-buffer )
  ("M-b" ibuffer )
  ("C-b" buffer-menu )
  ("d" kill-this-buffer :color red)
  ;; don't come back to previous buffer after delete
  ("C-d" (progn (kill-this-buffer) (next-buffer)) :color red)
  ("M-d" (progn (kill-this-buffer) (delete-window)) :color red)
  ("s" save-buffer :color red)
  ("." hydra-window/body :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-buffer-menu
  (:color pink
   :hint nil)
  "
^NAV^      ^MARK^         ^UNMARK^        ^ACTIONS^          ^SEARCH^
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

(defhydra hydra-dired-main (:color pink :hint nil :columns 4)
  "
^^^NAV^ ^^   ^EDIT^                ^MARK^      ^ACTION^
^ ^ _s_ ^ ^  _o_pen other window   _m_ark      _h_: show hidden
_c_ ^ ^ _r_  _R_ename              _u_nmark
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
  ("." nil "toggle hydra" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-dired-delete (:color pink :hint nil :columns 4)
  ("d" dired-flag-file-deletion "flag delete")
  ("x" dired-do-flagged-delete "DEL flagged")
  ("D" dired-do-delete "delete this")
  ("q" hydra-dired-main/body "back" :color blue))

(defhydra hydra-error ()
  ("t" next-error "next")
  ("n" next-error "next")
  ("s" previous-error "previous")
  ("p" previous-error "previous"))

(defhydra hydra-file (:hint nil :exit t)
  "
  ^FIND^           ^RECENT^      ^COMMAND^      ^BOOKMARKS^
  _f_: file        _r_: recent   _s_: save      _i_: init
_C-f_: other wdw _C-r_: fasd     _R_: rename    _k_: keybindings
  _p_: project      ^^           _d_: delete    _O_: org
  _o_: open         ^^            ^^            _F_: functions
   ^^               ^^            ^^            _t_: todo
"
  ("p" counsel-git)
  ("f" counsel-find-file)
  ("C-f" find-file-other-window)
  ("o" sam--open-in-external-app)
  ("r" ivy-switch-buffer)
  ("C-r" fasd-find-file)
  ("s" save-buffer)
  ("R" rename-file)
  ("d" sam--delete-current-buffer-file)
  ("k" (lambda () (interactive) (find-file "~/dotfile/emacs/keybindings.el")))
  ("i" (lambda () (interactive) (find-file "~/dotfile/emacs/init.el")))
  ("O" (lambda () (interactive) (find-file "~/dotfile/emacs/org.el")))
  ("F" (lambda () (interactive) (find-file "~/dotfile/emacs/functions.el")))
  ("t" sam--edit-todo))

(defhydra hydra-font (:hint nil :exit t)
  "
^MONO^   ^PROPORTIONAL^  ^VARIABLE
_g_: go  _i_: input      _r_: roboto

"
  ("g" (lambda () (interactive) (set-frame-font "Go Mono 12" t)))
  ("i" (lambda () (interactive) (set-frame-font "Input Sans 12" t) (text-scale-increase 1 :1)))
  ("r" (lambda () (interactive) (set-frame-font "Roboto 13"))))

(defhydra hydra-frame (:hint nil :columns 3 :color blue)
  "frames"
  ("d" delete-frame "delete")
  ("n" new-frame "new")
  ("D" delete-other-frames "delete other"))

(defhydra hydra-git
  (:body-pre (git-gutter-mode 1)
   :post (progn (kill-diff-buffers)
                (message "killed diff buffers"))
   :hint nil)
  "
^NAV^               ^HUNK^            ^FILES^        ^ACTIONS^
  _n_: next hunk    _s_tage hunk      _S_tage        _c_ommit
  _p_: prev hunk    _r_evert hunk     _R_evert       _b_lame
_C-P_: first hunk   _P_opup hunk      _d_iff         _C_heckout
_C-N_: last hunk    _R_evision start  _t_imemachine
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

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  ;; Ibuffer
  ;; this is genius hydra making from
  ;; https://github.com/abo-abo/hydra/wiki/Ibuffer
  "
     ^NAVIGATION^     ^MARK^          ^ACTIONS^          ^VIEW^
  _s_:    ↑        _m_: mark       _d_: delete        _g_: refresh
  _r_:  visit      _u_: unmark     _S_: save          _O_: sort
  _t_:    ↓        _*_: specific   _a_: all actions   _/_: filter
"
  ("t" ibuffer-forward-line)
  ("r" ibuffer-visit-buffer :color blue)
  ("s" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("d" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("O" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" ibuffer-quit "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
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

(defhydra hydra-insert (:hint nil :color blue)
  "
^INSERT^       ^LINK^    ^FINDER^
_t_: time      _m_: md   _M_: md
_s_: sentence  _o_: org  _O_: org
_p_: paragraph
"
  ("t" sam--insert-timestamp)
  ("m" (lambda () (interactive) (insert (grab-mac-link 'chrome 'markdown))))
  ("o" (lambda () (interactive) (insert (grab-mac-link 'chrome 'markdown))))
  ("M" (lambda () (interactive) (insert (grab-mac-link 'finder 'markdown))))
  ("O" (lambda () (interactive) (insert (grab-mac-link 'finder 'org))))

  ("s" lorem-ipsum-insert-sentences :color red)
  ("p" lorem-ipsum-insert-paragraphs :color red)
  ("q" nil "quit")
  ("." hydra-main/body "back"))

(defhydra hydra-launcher (:color blue :hint nil)
  "
^WEB^       ^BLOG^         ^EXPLORER^     ^APPS^
_g_oogle    _bn_ew post    _d_ired        _s_hell
_r_eddit    _bp_ublish     _D_eer         _S_hell gotodir
_w_iki      _bs_server     _r_anger       _a_pps
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

(defhydra hydra-make (:hint nil :columns 2)
  "
[_c_]: compile
"
  ("c" compile :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-quit (:hint nil :color blue)
  "
^QUIT^
_q_: emacs
_r_: restart
"
  ("q" save-buffers-kill-terminal)
  ("r" restart-emacs)
  ("." hydra-main/body "back"))

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                           :hint nil
                           :color pink
                           :post (deactivate-mark))
  "
  ^_s_^     _d_elete      _S_tring    _k_ill
_c_   _r_   _q_uit        _y_ank
  ^_t_^     _n_ew-copy    _R_eset
^^^^        _e_xchange    _u_ndo
^^^^        ^ ^
"
  ("c" backward-char)
  ("r" forward-char)
  ("s" previous-line)
  ("t" next-line)
  ("e" exchange-point-and-mark)
  ("n" copy-rectangle-as-kill)
  ("d" delete-rectangle)
  ("R" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("y" (lambda () (interactive) (save-excursion (yank-rectangle))))
  ("u" undo)
  ("S" string-rectangle)
  ("k" kill-rectangle)
  ("SPC" (lambda () (interactive) (rectangle-mark-mode 1)) "set")
  ("q" nil))

(defhydra hydra-term (:hint nil :exit t)
  "
^TERM^           ^MULTITERM^
_t_: term     |  _m_: multi  _c_: close
_e_: eshell   |  _n_: next   _o_: open
_s_: shell    |  _p_: prev   _S_: select
^ ^           |  ^ ^         _T_: toggle
"
  ("t" (lambda () (interactive) (term "/usr/local/bin/bash")))
  ("e" eshell)
  ("s" shell)
  ("m" multi-term)
  ("n" multi-term-next)
  ("p" multi-term-prev)
  ("o" multi-term-dedicated-open)
  ("c" multi-term-dedicated-close)
  ("S" multi-term-dedicated-select)
  ("T" multi-term-dedicated-toggle)
  ("q" nil :color blue))

(defhydra hydra-toggle (:hint nil :color blue)
  "
^THEMES^  ^MODES^        ^MODELINE^   ^FRAME^        ^LINE^
_d_ark    _f_lycheck     _T_ime       _F_ullscreen   _t_runcate
_l_ight   li_n_um        ^ ^          _m_aximized
^^        _w_hitespace   ^ ^          ^ ^
^^        _p_ersp-mode   ^ ^          ^ ^
"
  ("d" solarized-switch-to-dark)
  ("l" solarized-switch-to-light)
  ("f" flycheck-mode)
  ("n" nlinum-mode)
  ("T" display-time-mode)
  ("p" persp-mode)
  ("m" toggle-frame-maximized)
  ("F" toggle-frame-fullscreen)
  ("w" blank-mode :color red)
  ("t" toggle-truncate-lines)
  ("q" nil "quit" :color blue))

(defhydra hydra-window
  (:hint nil
   :color amaranth
   :columns 4
   :pre (winner-mode 1)
   :post (balance-windows))
  "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^ ^^^^   ^COMMAND^   ^WINDOW^
^ ^ _s_ ^ ^   _-_ : split H    ^ ^ _p_ ^ ^   _d_elete    ^1^ ^2^ ^3^ ^4^
_c_ _é_ _r_   _|_ : split V    _b_ ^=^ _f_   _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _t_ ^ ^   _h_ : split H    ^ ^ _n_ ^ ^   _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^ ^ ^ ^ ^   _R_edo
"
  ("c" windmove-left :color blue)
  ("r" windmove-right :color blue)
  ("t" windmove-down :color blue )
  ("s" windmove-up :color blue)

  ;; splt
  ("-" split-window-vertically)
  ("|" split-window-horizontally )
  ("v" split-window-horizontally :color blue)
  ("h" split-window-vertically :color blue)

  ;; size
  ("p" (lambda () (interactive) (enlarge-window -1)))
  ("b" enlarge-window-horizontally)
  ("f" (lambda () (interactive) (enlarge-window-horizontally -1)))
  ("n" (lambda () (interactive) (enlarge-window 1)))

  ("u" winner-undo)
  ("R" winner-redo)
  ("m" delete-other-windows )
  ("d" delete-window )

  ;; change height and width
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

  ("=" balance-windows )
  ("é" ace-window)
  ("." hydra-buffer/body "buffers" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-zoom (:hint nil)
  "
^BUFFER^       ^FRAME^        ^ACTION^
_t_: in        _T_: in        _0_: reset
_s_: out       _S_: out       _q_: quit
"
  ("t" zoom-in )
  ("s" zoom-out )
  ("T" zoom-frm-in )
  ("S" zoom-frm-out )
  ("0" zoom-frm-unzoom)
  ("q" nil :color blue))

(defhydra hydra-main (:hint nil :color blue)
  "
_a_: applications  _f_: file      _o_: outline   _T_: term
_b_: buffer        _i_: insert    _p_: project   _v_: git
_B_: frames        _j_: journal   _q_: quit      _z_: zoom
_é_: window        _m_: make      _t_: toggle
"
  ("a" hydra-launcher/body)
  ("b" hydra-buffer/body)
  ("B" hydra-frame/body)
  ("é" hydra-window/body)
  ("f" hydra-file/body)
  ("i" hydra-insert/body)
  ("j" org-capture)
  ("m" hydra-make/body)
  ("o" hydra-outline/body)
  ("p" hydra-projectile-if-projectile-p)
  ("q" hydra-quit/body)
  ("t" hydra-toggle/body)
  ("T" hydra-term/body)
  ("v" hydra-git/body)
  ("z" hydra-zoom/body)
  ("s-<tab>" other-frame)
  ("." nil "quit"))

(defhydra hydra-secondary (:hint nil :color blue)
  "
[^s-.^] :
_t_: todo
_a_: agenda
_f_: font
"
  ("t" (lambda () (interactive) (org-agenda 1 "t")))
  ("a" (lambda () (interactive) (org-agenda 1 "a")))
  ("f" hydra-font/body))
