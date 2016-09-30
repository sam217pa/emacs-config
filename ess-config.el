(use-package polymode :ensure t
  :mode
  (("\\.Rmd\\'" . poly-markdown+r-mode)))

(defhydra hydra-ess (:color teal :hint nil)
  "
     ^Commands^         ^info^         ^Load^         ^Navigate^
[_s_]: send         _d_: rdired     _l_: library   _o_: outline
_c_: list         _h_: help       _f_: file
_w_: set width  _C-h_: web-help
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

(add-hook 'ess-mode-hook (lambda () (setq-local outline-regexp "###-----------------")))

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

;; primary map, direct access
(general-define-key
 :states '(normal insert)
 :keymaps 'ess-mode-map
  "C-RET" 'ess-eval-region-or-line
  "M-RET" 'ess-eval-function-or-paragraph
  (general-chord ",,") 'hydra-ess/body
  (general-chord ",l") 'ess-eval-line
  (general-chord ",t") 'ess-eval-function-or-paragraph)

(setq ess-completing-read 'ivy-completing-read)

(setq ess-R-font-lock-keywords '((ess-R-fl-keyword:modifiers . t)
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
(setq ess-offset-continued 2          ; offset after first statement
      ess-expression-offset 2         ; offset for expression
      ess-nuke-trailing-whitespace-p t ;delete trailing whitespace
      ess-default-style 'DEFAULT) ; set default style for R source file

(add-to-list
 'aggressive-indent-dont-indent-if	     ; do not indent line if
 '(and (derived-mode-p 'ess-mode)	     ; in ess mode
       (null (string-match "\\(#+ .+ $\\)" ; and in a roxygen block
                           (thing-at-point 'line)))))


(defmacro if-looking-at-do-else (look-at does otherwise)
  `(if (looking-at ,look-at)
       (eval ,does)
     (eval ,otherwise)))

(defun sam--ess-newline ()
  "when behind a ), pressing RET will insert a new line and
leave the paren in place. "
  (interactive)
  (if-looking-at-do-else ")"
                         (progn (end-of-line) (ess-newline-and-indent))
                         (progn (ess-newline-and-indent) (message "cuicui"))))

(defun sam--ess-comma-after-paren ()
  " when behind a ), pressing `,` will step outside of the
paren and insert ,"
  (interactive)
  (if-looking-at-do-else ")"
                         (progn (forward-char) (insert ","))
                         (insert ",")))


(defun sam--double-hash-at-line-begin ()
  (interactive)
  (if-looking-at-do-else "^" (insert "## ") (insert "#")))

(general-define-key
 :states 'insert
 :keymaps 'ess-mode-map
  "#" 'sam--double-hash-at-line-begin
  "RET" 'sam--ess-newline
  "C-," 'sam--ess-comma-after-paren)

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
