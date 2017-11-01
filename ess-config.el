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

(use-package polymode :ensure t
  :mode
  (("\\.Rmd\\'" . poly-markdown+r-mode)))

(projectile-mode)

(use-package ess-R-data-view :ensure t
  :bind* (:map ess-mode-map
          ("C-c d" . ess-R-dv-ctable)
          ("C-c v" . ess-R-dv-pprint))
  )

(use-package ctable :ensure t)

(setq ess-swv-plug-into-AUCTeX-p t)

;; ---------- defaults ----------------------------------------------------
(setq ess-eval-visibly 'nowait)
(setq ess-roxy-insert-prefix-on-newline t)
(setq ess-eldoc-show-on-symbol t)
;; ess should use default completing-read, either ivy or helm.
(setq ess-use-ido nil)

(setq ess-R-font-lock-keywords
      '((ess-R-fl-keyword:modifiers . t)
        (ess-R-fl-keyword:fun-defs . t)
        (ess-R-fl-keyword:keywords . t)
        (ess-R-fl-keyword:assign-ops . t)
        (ess-R-fl-keyword:constants . t)
        (ess-fl-keyword:fun-calls . t)
        (ess-fl-keyword:numbers . t)
        (ess-fl-keyword:operators . t)
        (ess-fl-keyword:delimiters . t)
        (ess-fl-keyword:= . t)
        (ess-R-fl-keyword:F&T . t)
        (ess-R-fl-keyword:%op% . t)))


(ess-toggle-underscore nil)

(sp-local-pair 'ess-mode "%" "%")
;; when pressed RET after { or (,
;; {
;;    | <- cursor
;; }
(sp-local-pair 'ess-mode "{" nil
               :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
(sp-local-pair 'ess-mode "(" nil
               :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
;; usually i wanna type something in it, but not always.
(sp-local-pair 'ess-mode "(" ")")


;; ---------- function definition -----------------------------------------
(defun lesspy-backward-slurp ()
  "slurp sexp backward if at an opening paren"
  (interactive)
  (cond ((looking-back ess-closing-delim)
         (sp-backward-slurp-sexp))
        (t (self-insert-command arg))))

(defun lesspy-forward-slurp ()
  "slurp sexp forward if at a closing paren"
  (interactive)
  (cond ((looking-back ess-closing-delim)
         (sp-backward-down-sexp)
         (sp-slurp-hybrid-sexp)
         (sp-forward-sexp 2))
        (t (self-insert-command arg))))

(defun lesspy-kill-wip ()
  (interactive)
  (let ((beg (re-search-backward "^## \/\\*"))
        (end (re-search-forward "^## \\*\/")))
    (kill-region beg end)))

(defun ess-insert-pipe ()
  (interactive)
  (insert "%>% "))

;; ---------- font lock ---------------------------------------------------
(font-lock-add-keywords
 'ess-mode
 '(("^#' #.+$" . font-lock-warning-face)))


;; ---------- keybindings -------------------------------------------------

(general-define-key
 :keymaps 'ess-mode-map
  "RET" 'ess-newline-and-indent
  "C-RET" 'ess-eval-region-or-line
  "M-RET" 'ess-eval-function-or-paragraph
  "M-p"   'sp-backward-up-sexp
  "M-n"   'sp-up-sexp
  " " 'ess-insert-S-assign               ; shift alt space
  " " 'ess-insert-S-assign               ; shift space
  (general-chord ",z") 'ess-switch-to-inferior-or-script-buffer
  (general-chord ",,") 'hydra-ess/body
  (general-chord ",l") 'lesspy-eval-line
  (general-chord ",r") 'ess-eval-region-or-line-and-step
  (general-chord ",t") 'ess-eval-function-or-paragraph)

(general-define-key
 :keymaps 'inferior-ess-mode-map
  "s-d" 'ess-insert-pipe)

;; ---------- hydra -------------------------------------------------------

(defhydra hydra-ess (:color teal :hint nil)
  "
     ^Commands^        ^info^          ^Load^         ^Navigate^
     ^--------^        ^----^          ^----^         ^--------^
[_s_]: send         _d_: rdired     _l_: library   _o_: outline
_c_  : list         _h_: help       _f_: file
_w_  : set width  _C-h_: web-help
"
  ("s" hydra-ess-send/body)
  ("h" ess-help)
  ("c" ess-handy-commands)
  ("C-h" ess-help-web-search)
  ("d" ess-rdired)
  ("l" ess-load-library)
  ("f" ess-load-file)
  ("w" ess-execute-screen-options)
  ("q" nil "quit" :color blue)
  ("o" hydra-outline/body))


(defhydra hydra-ess-send (:color teal :hint nil)
  "
^  ^      ^Send^  ^Step^   ^Go^
^Line^     _l_     _C-l_   _M-l_
^Region^   _r_     _C-r_   _M-r_
^Chunk^    _c_     _C-c_   _M-c_
^Function^ _s_     _C-s_   _M-s_
"
  ("l" ess-eval-line)
  ("C-l" ess-eval-line-and-step :color red)
  ("M-l" ess-eval-line-and-go)

  ("r" ess-eval-region)
  ("C-r" ess-eval-region-and-step :color red)
  ("M-r" ess-eval-region-and-go)

  ("c" ess-eval-chunk)
  ("C-c" ess-eval-chunk-and-step :color red)
  ("M-c" ess-eval-chunk-and-go)

  ("s" ess-eval-function-or-paragraph)
  ("C-s" ess-eval-function-or-paragraph-and-step :color red)
  ("M-s" ess-eval-function-or-paragraph-and-go)

  ("q" nil "quit" :color blue))
