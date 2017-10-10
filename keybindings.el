;;; * Licence

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

;;; * Code

(use-package general :ensure t
  :config

;;; Shift

  

;;; Alt-
  (general-define-key
   "ð" 'kill-word-ap)

;;; C-
  (general-define-key
   "C-S-c" 'sp-splice-sexp
   "C-S-z" 'undo-tree-redo
   "C-S-k" 'kill-whole-line
   "C-é" 'hydra-window/body)

  (bind-key* "C-'" #'avy-goto-word-1)
  (bind-key* "C-." #'hydra-main/body)
  (bind-key* "C-/" #'complete-symbol)
  (bind-key* "C-¨" #'complete-symbol)   ; ctl-alt-i

;;; * C-M-

  

;;; * C-c

  (general-define-key
   :prefix "C-c"
   "v" 'magit-status
   "T" 'hydra-transparency/body)

;;; * C-x

  (general-define-key
   :prefix "C-x"
   "SPC" 'hydra-rectangle/body
   "d" 'dired-other-window
   "n" 'narrow-or-widen-dwim
   "p" 'hydra-projectile/body
   "o" 'other-window
   "=" 'balance-windows

   "C-b" 'ibuffer
   "C-r" 'ivy-switch-buffer
   "M-i" 'sam--insert-timestamp
   "M-c" 'compile)

;;; M-
  (general-define-key
   "M-<backspace>" 'delete-to-sentence-beg
   "M-é" 'ace-window
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-ê" 'hydra-error/body

   "M-s-n" 'forward-paragraph
   "M-s-p" 'backward-paragraph
   "M-s-i" 'shell-command-on-buffer)

;;; s-
  (general-define-key
   "s-<backspace>" 'ivy-switch-buffer
   "s-<tab>" 'sam--switch-to-other-buffer
   "s-c" 'windmove-left
   "s-r" 'windmove-right
   "s-d" 'kill-buffer-and-window
   "s-f" 'projectile-find-file
   "s-i" (lambda () (interactive) (save-excursion (mark-paragraph) (indent-region (region-beginning) (region-end))))
   "s-j" (lambda () (interactive) (join-line 4))
   "s-k" (lambda () (interactive)
           (save-excursion
             (move-beginning-of-line nil)
             (kill-visual-line -1)))    ; delete previous line
   "s-l" 'sam--comment-or-uncomment-region-or-line
   "s-o" 'sam--open-in-external-app
   "s-q" nil                          ; don't close emacs with option q.
   "s-t" nil                          ; don't show font panel with s-t.
   "s-u" 'negative-argument
   "s-w" 'delete-other-windows
   "s-n" 'narrow-or-widen-dwim
   "s-." 'hydra-secondary/body
   "s-(" 'hydra-sp/body)

;;; H-
  (general-define-key
   "H-<backspace>" 'ivy-switch-buffer-other-window
   "H-<tab>" 'hydra-outline/body
   "H-e" 'eshell-here
   "H-f" 'toggle-frame-fullscreen
   "H-i" 'sam/insert-filename
   "H-l" 'sam--duplicate-line
   "H-n" 'make-frame
   "H-s" 'move-text-up
   "H-t" 'move-text-down
   "H-r" 'counsel-recentf
   "H-u" 'revert-buffer

   ;; H-M-
   "H-M-p" 'scroll-up-command
   "H-M-n" 'scroll-down-command
   "H-M-s" 'mark-sexp)

;;; Key chords
  (use-package key-chord :ensure t
    :config
    (key-chord-mode 1)
    (setq key-chord-two-keys-delay 0.2)
    (use-package key-seq :ensure t
      :config
      (key-seq-define-global "««" (lambda () (interactive) (insert "<")))
      (key-seq-define-global "»»" (lambda () (interactive) (insert ">")))
      (key-seq-define-global "xq" 'hydra-context/body)
      (key-seq-define-global "qb" #'counsel-bookmark)
      (key-seq-define-global "qd" #'kill-this-buffer)
      (key-seq-define-global "ql" #'avy-goto-line)))

;;; Mode specific map
  (general-define-key :keymaps 'Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  (general-define-key :keymaps 'emacs-lisp-mode-map "s-e" 'eval-defun)
  (general-define-key :keymaps 'shell-mode-map "H-c" 'erase-buffer)
  (general-define-key :keymaps 'term-mode-map "H-c" 'erase-buffer)
  (general-define-key
   :keymaps 'compilation-mode-map
   "t" 'compilation-next-error
   "s" 'compilation-previous-error
   "r" 'compile-goto-error))

;;; * Which-key

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

;;; * Global

(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap fill-paragraph] #'sam/fill-or-unfill)
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'sam/end-of-code-or-line)
(global-set-key (kbd "C-x C-S-e") #'eval-and-replace)

(global-set-key (kbd "<f5>") 'mu4e)
(global-set-key (kbd "<f6>") 'elfeed)
(global-set-key (kbd "<f7>") 'org-capture)
(global-set-key (kbd "<f8>") 'org-agenda)
(global-set-key (kbd "<f9>") 'bongo)

;;; * HYDRA

(defhydra hydra-dired-main (:color pink :hint nil :columns 4)
  "
^^^NAV^ ^^   ^EDIT^                ^MARK^      ^ACTION^
^ ^ _s_ ^ ^  _o_pen other window   _m_ark        _h_: show hidden
_c_ ^ ^ _r_  _R_ename              _u_nmark      _'_: eshell
^ ^ _t_ ^ ^  _S_ort                _d_elete    _C-'_: shell
"
  ("t" dired-next-line :color red)
  ("s" dired-previous-line :color red)
  ("r" dired-find-file :color blue)
  ("c" dired-up-directory :color red)
  ("o" dired-find-file-other-window :color blue)
  ("R" dired-rename-file)
  ("S" hydra-dired-sort/body :color blue)
  ("u" dired-unmark)
  ("m" dired-mark)
  ("d" hydra-dired-delete/body :color blue)
  ("h" dired-omit-mode)
  ("'" eshell-here :color blue)
  ("C-'" shell :color blue)
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

(defun counsel-font ()
  "Change font of current frame"
  (interactive)
  (ivy-read "Chose font :"
            (font-family-list)
            :caller 'counsel-font
            :action (lambda (x) (set-frame-font x))))

(defhydra hydra-frame (:hint nil :columns 3 :color blue)
  "frames"
  ("d" delete-frame "delete")
  ("n" new-frame "new")
  ("D" delete-other-frames "delete other"))

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
  ("g" (browse-url "https://www.google.fr/"))
  ("R" (browse-url "http://www.reddit.com/r/emacs/"))
  ("w" (browse-url "http://www.emacswiki.org/"))
  ("t" (browse-url "https://twitter.com/?lang=fr"))
  ("s" (sam--iterm-focus))
  ("S" (sam--iterm-goto-filedir-or-home))
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
  ;; ("d" (lambda () (interactive) (load-theme 'apropospriate-dark t)))

  ("d" (lambda () (interactive) (solarized--dark-or-light 'dark)
         (set-face-attribute 'default nil :font my-font-for-dark)))
  ("l" (lambda () (interactive) (solarized--dark-or-light 'light)
             (set-face-attribute 'default nil :font my-font-for-light)))
  ("f" flycheck-mode)
  ("n" nlinum-mode)
  ("T" display-time-mode)
  ("p" persp-mode)
  ("m" toggle-frame-maximized)
  ("F" toggle-frame-fullscreen)
  ("w" blank-mode :color red)
  ("t" toggle-truncate-lines)
  ("q" nil "quit" :color blue))

(defhydra hydra-transparency
  (:columns 2
   :body-pre
   (let* ((alpha (frame-parameter (selected-frame) 'alpha)))
     (cond ((not alpha) (sam--set-transparency -10))
           ((eql alpha 100) (sam--set-transparency -10))
           (t nil))))
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("t" (lambda () (interactive) (sam--set-transparency +1)) "+ more")
  ("s" (lambda () (interactive) (sam--set-transparency -1)) "- less")
  ("T" (lambda () (interactive) (sam--set-transparency +10)) "++ more")
  ("S" (lambda () (interactive) (sam--set-transparency -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

(defhydra hydra-window-enlarge (:hint nil)
  "
^SIZE^ ^^^^
^ ^ ^ ^ _S_ ^ ^
^ ^ ^ ^ _s_ ^ ^
_C_ _c_ ^ ^ _r_ _R_
^ ^ ^ ^ _t_ ^ ^
^ ^ ^ ^ _T_ ^ ^
^ ^ ^ ^ ^ ^ ^ ^
"
  ("s" (lambda () (interactive) (enlarge-window -1)))
  ("S" (lambda () (interactive) (enlarge-window -10)))
  ("c" enlarge-window-horizontally)
  ("C" (lambda () (interactive) (enlarge-window-horizontally 10)))
  ("r" (lambda () (interactive) (enlarge-window-horizontally -1)))
  ("R" (lambda () (interactive) (enlarge-window-horizontally -10)))
  ("t" (lambda () (interactive) (enlarge-window 1)))
  ("T" (lambda () (interactive) (enlarge-window 10)))
  ("q" nil :color blue))

(defhydra hydra-window
  (:hint nil
   :color amaranth
   :columns 4
   :pre (winner-mode 1)
   :post (balance-windows))
  "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^   ^COMMAND^   ^WINDOW^
^ ^ _s_ ^ ^   _-_ : split H    ^ ^     _d_elete    ^1^ ^2^ ^3^ ^4^
_c_ _é_ _r_   _|_ : split V    _e_     _m_aximize  ^5^ ^6^ ^7^ ^8^
^ ^ _t_ ^ ^   _h_ : split H    ^ ^     _u_ndo      ^9^ ^0^
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^     _R_edo
"
  ("c" windmove-left :color blue)
  ("r" windmove-right :color blue)
  ("t" windmove-down :color blue)
  ("s" windmove-up :color blue)

  ;; splt
  ("-" split-window-vertically)
  ("|" split-window-horizontally)
  ("v" split-window-horizontally :color blue)
  ("h" split-window-vertically :color blue)

  ;; size
  ("e" hydra-window-enlarge/body :color blue)

  ("u" winner-undo)
  ("R" winner-redo)
  ("m" delete-other-windows)
  ("d" delete-window)

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

  ("=" balance-windows)
  ("é" ace-window)
  ("." hydra-buffer/body "buffers" :color blue)
  ("'" hydra-tile/body "tile" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-zoom (:hint nil)
  "
^BUFFER^   ^FRAME^    ^ACTION^
_t_: +     _T_: +     _0_: reset
_s_: -     _S_: -     _q_: quit
 ^^[_-_] [_+_]
"
  ("t" zoom-in)
  ("s" zoom-out)
  ("T" zoom-frm-in)
  ("S" zoom-frm-out)
  ("0" zoom-frm-unzoom)
  ("-" zoom-out)
  ("+" zoom-in)
  ("q" nil :color blue))

;; ---------- MAIN HYDRA --------------------------------------------------

(defhydra hydra-main (:hint nil :color teal)
  "
_a_: applications  _f_: file      _o_: outline   _T_: term
_b_: buffer        _i_: insert    _p_: project   _v_: git
_B_: frames        _j_: journal   _Q_: quit      _z_: zoom
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
  ("Q" hydra-quit/body)
  ("t" hydra-toggle/body)
  ("T" hydra-term/body)
  ("v" hydra-git/body)
  ("z" hydra-zoom/body)
  ("s-<tab>" other-frame)
  ("<tab>" hydra-secondary/body "secondary")
  ("q" (message "Quit Primary Hydra") "quit" :color blue))

(defhydra hydra-secondary (:hint nil :color teal)
  "
_a_: agenda  _b_: bongo
_f_: font
_t_: todo
"
  ("a" (lambda () (interactive) (org-agenda 1 "a")))
  ("b" hydra-bongo/body)
  ("f" hydra-font/body)
  ("t" (lambda () (interactive) (org-agenda 1 "t")))
  ("<tab>" hydra-main/body "primary")
  ("q" (message "Abort") "abort" :color blue))


(defhydra hydra-context (:hint nil :color teal)
  "
^XQ^
_s_: semantic
"
  ("s" counsel-semantic)
  ("i" nil :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("p" (yank-pop 1) "next")
  ("n" (yank-pop -1) "prev")
  ("l" counsel-yank-pop "list" :color blue))
(bind-key* "C-y" #'hydra-yank-pop/yank)

(defhydra hydra-goto (:pre (nlinum-mode 1)
                      :post (nlinum-mode -1)
                      :hint nil)
  "
^GOTO^      ^ERROR^     ^DUMB-JUMP^
_c_: char   _p_: prev   _g_: go     _SPC_: quick look
_l_: line   _n_: next   _G_: go OW  _b_: back
"
  ("c" goto-char)
  ("n" next-error)
  ("p" previous-error)
  ("l" goto-line)
  ("g" dumb-jump-go)
  ("G" dumb-jump-go-other-window)
  ("SPC" dumb-jump-quick-look)
  ("b" dumb-jump-back)
  ("q" nil "quit" :color blue))
(bind-key* "M-g" #'hydra-goto/body)

(defhydra hydra-mark (:exit t
                      :columns 3
                      :idle 1.0)
  "Mark"
  ("f" er/mark-defun "function")
  ("w" er/mark-word "word")
  ("u" er/mark-url "url")
  ("e" mark-sexp "sexp")
  ("E" er/mark-email "Email")
  ("b" hydra-mark-buffer/body "Buffer")
  ("l" mark-line "Line")
  ("p" er/mark-text-paragraph "paragraph")
  ("s" er/mark-symbol "symbol")
  ("S" er/mark-symbol-with-prefix "Prefixed symbol")
  ("q" er/mark-inside-quotes "Inside quotes")
  ("Q" er/mark-outside-quotes "Outside quotes")
  ("(" er/mark-inside-pairs "Inside pairs")
  ("[" er/mark-inside-pairs "Inside pairs")
  ("{" er/mark-inside-pairs "Inside pairs")
  (")" er/mark-outside-pairs "Outside pairs")
  ("]" er/mark-outside-pairs "Outside pairs")
  ("}" er/mark-outside-pairs "Outside pairs")
  ("t" er/mark-inner-tag "Inner tag")
  ("T" er/mark-outer-tag "Outer tag")
  ("c" er/mark-comment "Comment")
  ("." er/expand-region "Expand region" :exit nil)
  ("," er/contract-region "Contract region" :exit nil))
(bind-key* "s-SPC" #'hydra-mark/body)

(defhydra hydra-kbd-macro (:color blue :hint nil :columns 3)
  "KBD MACRO"
  ("." call-last-kbd-macro "redo last" :color red)
  ("s" start-kbd-macro "start")
  ("e" end-kbd-macro "end")
  ("q" nil :color blue "quit"))
(bind-key* "C-x q" #'hydra-kbd-macro/body)
