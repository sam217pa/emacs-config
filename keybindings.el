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

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))

(use-package key-seq
  :ensure t
  :after key-chord
  :config
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qd" #'kill-this-buffer))

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
   "C-é"   'hydra-window/body)

  (bind-keys*
   ("C-'" . avy-goto-word-1)
   ("C-/" . complete-symbol))


;;; * C-M-

  

;;; * C-c

  (general-define-key
   :prefix "C-c"
   "v" 'magit-status)

;;; * C-x

  (general-define-key
   :prefix "C-x"
   "d" 'dired-other-window
   "n" 'narrow-or-widen-dwim
   "=" 'balance-windows
   "C-b" 'ibuffer
   "M-c" 'compile)

;;; M-
  (general-define-key
   "M-<backspace>" 'sam|delete-to-sentence-beg
   "M-é" 'ace-window
   "M-/" 'hippie-expand
   "M-«" 'beginning-of-buffer
   "M-»" 'end-of-buffer
   "M-ê" 'hydra-error/body

   "M-P" 'scroll-other-window
   "M-N" 'scroll-other-window-down

   "M-s-n" 'forward-paragraph
   "M-s-p" 'backward-paragraph
   "M-s-i" 'shell-command-on-buffer
   "M-SPC" 'cycle-spacing)



;;; s-

  (general-define-key
   "s-<tab>" 'sam|switch-to-other-buffer
   "s-c" 'windmove-left
   "s-r" 'windmove-right
   "s-d" 'kill-buffer-and-window
   "s-f" 'projectile-find-file
   "s-i" 'sam|indent-paragraph
   "s-j" 'sam|join-to-next-line
   "s-l" 'sam|comment-or-uncomment-region-or-line
   "s-o" 'sam|open-in-external-app
   "s-q" 'sam|unfill-paragraph
   "s-t" 'ivy-switch-buffer          ; don't show font panel with s-t.
   "s-u" 'negative-argument
   "s-w" 'sam|main-window
   "s-n" 'narrow-or-widen-dwim)



;;; H-

  (general-define-key
   "H-<tab>" 'hydra-outline/body
   "H-w" 'sam|maximize-window
   "H-l" 'sam--duplicate-line
   "H-n" 'make-frame
   "H-s" 'move-text-up
   "H-t" 'move-text-down
   "H-u" 'revert-buffer
   "H-." 'sam|finder-here
   "H-'" 'sam|iterm-here

   ;; H-M-
   "H-M-p" 'scroll-up-command
   "H-M-n" 'scroll-down-command
   "H-M-s" 'mark-sexp)

  

;;; Mode specific map

  (general-define-key
   :keymaps 'compilation-mode-map
   "t" 'compilation-next-error
   "s" 'compilation-previous-error
   "r" 'compile-goto-error))

;;; * Global

(global-set-key (kbd "C-x C-S-e") #'eval-and-replace)

(bind-keys*
 ("C-c <left>" . hydra-winner/winner-undo)
 ("C-c <right" . hydra-winner/winner-redo))

;;; * HYDRA

(defhydra hydra-insert (:hint nil :color blue)
  "
^INSERT^       ^LINK^    ^FINDER^
_t_: time      _m_: md   _M_: md
_s_: sentence  _o_: org  _O_: org
_p_: paragraph
"
  ("t" sam--insert-timestamp)
  ("m" (lambda () (interactive) (insert (grab-mac-link 'firefox 'markdown))))
  ("o" (lambda () (interactive) (insert (grab-mac-link 'firefox 'org))))
  ("M" (lambda () (interactive) (insert (grab-mac-link 'finder 'markdown))))
  ("O" (lambda () (interactive) (insert (grab-mac-link 'finder 'org))))

  ("s" lorem-ipsum-insert-sentences :color red)
  ("p" lorem-ipsum-insert-paragraphs :color red)
  ("q" nil "quit"))

(defhydra hydra-toggle (:hint nil :color blue :columns 1)
  "TOGGLE"
  ("f" flycheck-mode "flycheck")
  ("n" nlinum-mode "nlinum")
  ("w" blank-mode "whitespace" :color red )
  ("t" toggle-truncate-lines "truncate lines"))

(defhydra hydra-transparency
  (:columns 2
   :body-pre
   (let* ((alpha (frame-parameter (selected-frame) 'alpha)))
     (cond ((not alpha) (sam|set-transparency -10))
           ((eql alpha 100) (sam|set-transparency -10))
           (t nil))))
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("t" (lambda () (interactive) (sam|set-transparency +1)) "+ more")
  ("s" (lambda () (interactive) (sam|set-transparency -1)) "- less")
  ("T" (lambda () (interactive) (sam|set-transparency +10)) "++ more")
  ("S" (lambda () (interactive) (sam|set-transparency -10)) "-- less")
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
   :pre (winner-mode 1))
  "
^MOVE^ ^^^^   ^SPLIT^          ^SIZE^   ^COMMAND^
^ ^ _s_ ^ ^   _-_ : split H    ^ ^     _d_elete
_c_ _é_ _r_   _|_ : split V    _e_     _m_aximize
^ ^ _t_ ^ ^   _h_ : split H    ^ ^     _u_ndo
^ ^ ^ ^ ^ ^   _v_ : split V    ^ ^     _R_edo
"
  ("c" windmove-left :color blue)
  ("r" windmove-right :color blue)
  ("t" windmove-down :color blue)
  ("s" windmove-up :color blue)

  ;; split
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

  ("=" balance-windows)
  ("é" ace-window)
  ("q" nil "quit" :color blue))

(setq text-scale-mode-step 1.05)
(defhydra hydra-zoom (:hint nil)
  "ZOOM"
  ("t" text-scale-increase "+")
  ("s" text-scale-decrease "-")
  ("q" nil :color blue))

(defhydra hydra-winner (:color red :hint nil)
  "WINNER"
  ("<left>" winner-undo "undo")
  ("<right>" winner-redo "redo"))

;;; ** MAIN HYDRA

(general-define-key
 :prefix "C-."
 "a" '(hydra-transparency/body :wk "alpha")
 "i" '(hydra-insert/body       :wk "insert")
 "t" '(hydra-toggle/body       :wk "toggle")
 "w" '(hydra-window/body       :wk "window")
 "z" '(hydra-zoom/body         :wk "zoom")
 "t" '(hydra-thesis/body       :wk "thesis")
 "e" '(hydra-errors/body      :wk "errors")
 "d" '(hydra-define/body :wk "defines"))

(defhydra hydra-define (:hint nil :color blue)
  "
         ^At Point^   ^Input^
defines: _w_          _C-w_
diction: _d_          _C-d_
synonym: _s_          _C-s_
wiki   : _W_"
  ("w" define-word-at-point)
  ("d" osx-dictionary-search-word-at-point)
  ("s" powerthesaurus-lookup-word-at-point)
  ("W" wiki-summary)
  ("C-w" define-word)
  ("C-d" osx-dictionary-search-input)
  ("C-s" powerthesaurus-lookup-word))

(defhydra hydra-ui (:hint nil :color teal :columns 1)
  ("a" hydra-transparency/body "alpha")
  ("t" hydra-toggle/body "toggle")
  ("z" hydra-zoom/body "zoom"))

(defhydra hydra-yank-pop (:hint nil)
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("p" (yank-pop 1) "next")
  ("n" (yank-pop -1) "prev")
  ("l" counsel-yank-pop "list" :color blue))

(bind-key* "C-y" #'hydra-yank-pop/yank)
(bind-key* "M-y" #'hydra-yank-pop/yank-pop)

;; taken from "http://joaotavora.github.io/yasnippet/snippet-development.html#sec-3-2
;; useful when resolving merge conflicts
(defhydra hydra-smerge (:color pink
                        :hint nil
                        :pre (smerge-mode 1)
                        ;; Disable `smerge-mode' when quitting hydra if
                        ;; no merge conflicts remain.
                        :post (smerge-auto-leave))
  "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("\C-m" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("C" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("q" nil "cancel" :color blue))

(defhydra hydra-thesis (:color blue)
  "
THESIS
"
  ("p" sam|thesis-projects "projects")
  ("t" sam|thesis-thesis "thesis"))

(defun sam|thesis-thesis ()
  "Find file inside thesis folder."
  (interactive)
  (counsel-find-file "~/these/these"))

(defhydra hydra-page (:hint nil)
  "PAGE"
  ("t" forward-page "next")
  ("s" backward-page "previous"))

(bind-keys*
 ("C-x [" . hydra-page/backward-page)
 ("C-x ]" . hydra-page/forward-page))

(provide 'keybindings)
;;; keybindings.el ends here
