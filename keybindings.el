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
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define-global "xq" #'ryo-modal-mode))

(use-package key-seq
  :ensure t
  :after key-chord
  :config
  (key-seq-define-global "««" (lambda () (interactive) (insert "<")))
  (key-seq-define-global "»»" (lambda () (interactive) (insert ">")))
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qd" #'kill-this-buffer)
  (key-seq-define-global "qr" (lambda () (interactive) (set-mark-command 4))))

(use-package ryo-modal
  :ensure t
  :delight
  :commands ryo-modal-mode
  :init
  (add-hook 'ryo-modal-mode-hook
            (lambda ()
              (if ryo-modal-mode
                  (selected-minor-mode 1)
                (selected-minor-mode -1))
              (when (string-equal "emacs-lisp-mode" major-mode)
                (if ryo-modal-mode (lispy-mode -1) (lispy-mode 1)))
              (when (string-equal "ess-mode" major-mode)
                (if ryo-modal-mode (lesspy-mode -1) (lesspy-mode 1)))))
  :config
  ;; nicer which-key printing
  (push '((nil . "ryo:.*:") . (nil . "")) which-key-replacement-alist)

  (setq ryo-modal-cursor-color "#268bd2")
  (setq ryo-modal-cursor-type 'box)

  (let ((text-objects '(("w" er/mark-word :name "Word")
                        ("i" er/mark-inside-pairs :name "In pairs")
                        ("o" er/mark-outside-pairs :name "Out pairs")
                        ("d" er/mark-defun :name "Defun")
                        ("p" er/mark-paragraph :name "Paragraph")
                        ("s" er/mark-sentence :name "Sentence")))
        (avy-destinations '(("l" avy-goto-line :name "line")
                            ("w" avy-goto-word-1 :name "word")
                            ("c" avy-goto-char :name "char"))))
    (eval `(ryo-modal-keys
            ("." ryo-modal-repeat)
            ("/" swiper)
            ("0" "M-0")
            ("1" "M-1")
            ("2" "M-2")
            ("3" "M-3")
            ("4" "M-4")
            ("5" "M-5")
            ("6" "M-6")
            ("7" "M-7")
            ("8" "M-8")
            ("9" "M-9")
            ("<tab>" sam|switch-to-other-buffer :name "other buffer")
            ("A" sam|end-of-code-or-line :exit t)
            ("B" counsel-bookmark)
            ("I" smarter-move-beginning-of-line :exit t)
            ("O" previous-line :then '(end-of-line newline-and-indent) :exit t)
            ("U" sam|redo)
            ("J" sam|join-to-next-line)
            ("b" ivy-switch-buffer)
            ("c" backward-char)
            ("f" counsel-find-file)
            ("h" ,text-objects :then '(kill-region) :exit t)
            ("i" ryo-modal-mode)
            ("j" ,avy-destinations)
            ("k" ,text-objects :then '(kill-region))
            ("o" end-of-line :then '(newline-and-indent) :exit t)
            ("p" hydra-project/body :name "project" :exit t)
            ("r" forward-char)
            ("s" previous-line)
            ("t" next-line)
            ("u" sam|undo)
            ("v" ,text-objects)
            ("w" hydra-window/body :name "window")
            ("y" hydra-yank-pop/yank :name "yank"))))

  (ryo-modal-key
   "SPC" '(("a" org-agenda :name "agenda")
           ("b" ibuffer-list-buffers :name "buffer list")
           ("c" org-capture :name "capture")
           ("d" dired-jump :name "dired")
           ("g" magit-status)
           ("m" mu4e :name "mail")
           ("n" elfeed :name "news")
           ("s" save-buffer :name "save"))))

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
   "v" 'magit-status)

;;; * C-x

  (general-define-key
   :prefix "C-x"
   "SPC" 'hydra-rectangle/body
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
   "M-s-i" 'shell-command-on-buffer)

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
   "s-w" 'delete-other-windows
   "s-n" 'narrow-or-widen-dwim)

;;; H-
  (general-define-key
   "H-<tab>" 'hydra-outline/body
   "H-w" 'toggle-frame-maximized
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

;;; Key chords



;;; Mode specific map
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
  "C-c @"   "hide-show"
  "M-SPC h" "info"
  "M-SPC g" "grep"
  "M-SPC M-s" "occur")

;;; * Global

(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))
(global-set-key [remap move-beginning-of-line] #'smarter-move-beginning-of-line)
(global-set-key [remap move-end-of-line] #'sam|end-of-code-or-line)
(global-set-key (kbd "C-x C-S-e") #'eval-and-replace)

(global-set-key (kbd "<f5>") 'mu4e-new-frame)
(global-set-key (kbd "<f6>") 'elfeed)
(global-set-key (kbd "<f7>") 'org-capture)
(global-set-key (kbd "<f8>") 'org-agenda)
(bind-keys*
 ("C-c <left>" . hydra-winner/winner-undo)
 ("C-c <right" . hydra-winner/winner-redo))

;;; * HYDRA

(defhydra hydra-frame (:hint nil :columns 3 :color blue)
  "frames"
  ("d" delete-frame "delete")
  ("n" new-frame "new")
  ("D" delete-other-frames "delete other"))

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
  ("q" nil "quit")
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

(defhydra hydra-main (:hint nil :color teal :columns 1)
  "MAIN"
  ("p" hydra-project/body "projects")
  ("u" hydra-ui/body "UI")
  ("w" hydra-window/body "window")
  ("i" hydra-insert/body "insert")
  ("e" hydra-text/body "text")
  ("s" hydra-search/body "search")
  ("g" hydra-gtd/body "GTD")
  ("q" nil "quit" :color blue))

(defhydra hydra-ui (:hint nil :color teal :columns 1)
  ("a" hydra-transparency/body "alpha")
  ("t" hydra-toggle/body "toggle")
  ("z" hydra-zoom/body "zoom"))

(defhydra hydra-gtd (:hint nil :color teal :columns 1)
  ("a" org-agenda "agenda")
  ("c" org-capture "capture"))

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

(defhydra hydra-text (:color red :hint nil :columns 1)
  "TEXT"
  ("i" hydra-indent/body "indent" :color blue ))

(defhydra hydra-indent (:color red :hint nil :columns 1)
  "
MARK
_l_ine
_p_ara
_b_uff
"
  ;; mark
  ("l" mark-line)
  ("p" mark-paragraph)
  ("b" mark-whole-buffer)
  ;; indent
  ("i" sam|indent-region "indent" :color blue))

(defhydra hydra-search (:color teal :hint nil :columns 1)
  "SEARCH"
  ("b" swiper "buffer"))

(defhydra hydra-project (:color teal :hint nil :columns 1 :body-pre (projectile-mode))
  "PROJECTS"
  ("p" counsel-projectile-switch-project "project switch")
  ("s" counsel-projectile-rg "project search")
  ("c" projectile-compile-project "project compile")
  ("f" counsel-projectile-find-file "find file"))
