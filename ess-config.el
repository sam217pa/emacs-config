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

;; ---------- defaults ----------------------------------------------------
(setq ess-completing-read 'ivy-completing-read)

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
 '(("^##' #.+$" . font-lock-warning-face)))


;; ---------- keybindings -------------------------------------------------

(general-define-key
 :keymaps 'ess-mode-map
  "C-RET" 'ess-eval-region-or-line
  "M-RET" 'ess-eval-function-or-paragraph
  "M-p"   'sp-backward-up-sexp
  "M-n"   'sp-up-sexp
  " " 'ess-insert-S-assign               ; shift alt space
  " " 'ess-insert-S-assign               ; shift space
  (general-chord ",z") 'ess-switch-to-inferior-or-script-buffer
  (general-chord ",,") 'hydra-ess/body
  (general-chord ",l") 'ess-eval-line
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
