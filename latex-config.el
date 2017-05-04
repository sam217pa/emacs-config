

(use-package company-auctex :ensure t
  :config
  (append-to-list 'company-backends
                  '(company-auctex-labels
                    company-auctex-bibs
                    company-auctex-environments
                    company-auctex-macros
                    company-auctex-symbols)))

(use-package auctex-latexmk :ensure t
  :config
  (auctex-latexmk-setup))

(load "preview-latex.el" nil t t)

(defvar latex-nofill-env '("equation" "equation*" "align" "align*" "tabular" "tikzpicture")
  "List of environment names in which `auto-fill-mode' will be inhibited.")

(defvar latex-build-command (if (executable-find "latexmk") "LatexMk" "LaTeX")
  "the default command to use to build the document")

(defun latex--autofill ()
  "Check whether the pointer is currently inside one of the
    environments described in `latex-nofill-env' and if so, inhibits
    the automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment latex-nofill-env))))
    (when do-auto-fill
      (do-auto-fill))))

(defun latex-auto-fill-mode ()
  "Toggle auto-fill-mode using the custom auto-fill function."
  (interactive)
  (auto-fill-mode)
  (setq auto-fill-function 'latex--autofill))

;; from spacemacs
(setq
 TeX-command-default latex-build-command
 TeX-auto-save t
 TeX-parse-self t
 TeX-syntactic-comment t
 TeX-source-correlate-start-server nil  ; synctex support
 LaTeX-fill-break-at-separators nil     ; Don't insert line-break at inline math
 )

(setq-default LaTeX-item-indent 0)

(setq-default
 TeX-engine 'default
 TeX-PDF-mode t                         ; pdf output by default
 )

;; keybindings

;; Rebindings for TeX-font
(defun latex/font-bold () (interactive) (TeX-font nil ?\C-b))
(defun latex/font-medium () (interactive) (TeX-font nil ?\C-m))
(defun latex/font-code () (interactive) (TeX-font nil ?\C-t))
(defun latex/font-emphasis () (interactive) (TeX-font nil ?\C-e))
(defun latex/font-italic () (interactive) (TeX-font nil ?\C-i))
(defun latex/font-clear () (interactive) (TeX-font nil ?\C-d))
(defun latex/font-calligraphic () (interactive) (TeX-font nil ?\C-a))
(defun latex/font-small-caps () (interactive) (TeX-font nil ?\C-c))
(defun latex/font-sans-serif () (interactive) (TeX-font nil ?\C-f))
(defun latex/font-normal () (interactive) (TeX-font nil ?\C-n))
(defun latex/font-serif () (interactive) (TeX-font nil ?\C-r))
(defun latex/font-oblique () (interactive) (TeX-font nil ?\C-s))
(defun latex/font-upright () (interactive) (TeX-font nil ?\C-u))

(defun TeX-newline-and-indent-region ()
  (interactive)
  (TeX-newline)
  (outline-indent-subtree)
  (indent-according-to-mode))

(global-set-key [remap TeX-newline] #'TeX-newline-and-indent-region)

(general-define-key
 :keymaps 'LaTeX-mode-map
  "C-," 'hydra-latex/body
  "M-q" 'hydra-latex-fill/body)

(defhydra hydra-latex (:color blue :hint nil :columns 3)
  "
Latex

   ^Insert^        ^Command^     ^ ^                  ^Navigate^
   ^---------^     ^-------^-----------------^^       ^--------^
_e_: env     │  _;_: comment     _v_: view    │  _M-g_: error
_m_: macro   │  _c_: command on  _M-q_: fill  │  _TAB_: outline
_t_: font    │  _p_: preview
_s_: section │
"
  ("t" hydra-latex-font/body )
  ("m" TeX-insert-macro )
  ("e" LaTeX-environment )
  ("s" LaTeX-section )
  ("p" hydra-latex-preview/body )
  ("c" hydra-latex-command/body )
  ("\;" hydra-latex-comment/body )
  ("M-g" hydra-latex-error/body )
  ("M-q" hydra-latex-fill/body )
  ("TAB" hydra-outline/body )
  ("v" TeX-view )
  ("q" nil "quit" :color blue))

(defhydra hydra-latex-comment (:color amaranth :hint nil :colums 2)
  "Comment or uncomment"
  ("r" TeX-comment-or-uncomment-region "region")
  ("p" TeX-comment-or-uncomment-paragraph "para")
  ("." hydra-latex/body "back" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-latex-command (:color teal :hint nil :columns 3)
  "Run Latex Command on"
  ("b" TeX-command-buffer "buffer")
  ("m" TeX-command-master "master" )
  ("r" TeX-command-region "region" )
  ("a" TeX-command-run-all "all" )
  ("s" LaTeX-command-section "section"))

(defhydra hydra-latex-fill (:color blue :hint nil :columns 2)
  "Latex Fill"
  ("r" LaTeX-fill-region "region" )
  ("s" LaTeX-fill-section "section")
  ("p" LaTeX-fill-paragraph "para")
  ("e" LaTeX-fill-environment "environment")
  ("C-r" LaTeX-fill-region-as-paragraph "reg as par")
  ("C-p" LaTeX-fill-region-as-para-do "reg as par do")
  ("." hydra-latex/body "back" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-latex-error (:color red :hint nil :columns 2)
  "Latex Errors"
  ("n" TeX-next-error "next")
  ("t" TeX-next-error "next")
  ("p" TeX-previous-error "prev")
  ("s" TeX-previous-error "prev")
  ("." hydra-latex/body "back" :color blue)
  ("q" nil "quit" :color blue))

(defhydra hydra-latex-font (:color blue :hint nil)
  "
^Latex Font^
^----------^
_b_: bold           _M_: small-caps
_m_: medium         _S_: sans-serif
_c_: code           _s_: serif
_e_: emphasis       _n_: normal
_i_: italic         _o_: oblique
_C_: clear          _u_: upright
_l_: calligraphic

[_._]: back [_q_]: quit
"
    ("b" latex/font-bold)
    ("c" latex/font-code)
    ("C" latex/font-clear)
    ("e" latex/font-emphasis)
    ("i" latex/font-italic)
    ("l" latex/font-calligraphic)
    ("m" latex/font-medium)
    ("M" latex/font-small-caps)
    ("n" latex/font-normal)
    ("o" latex/font-oblique)
    ("s" latex/font-serif)
    ("S" latex/font-sans-serif)
    ("u" latex/font-upright)
    ("." hydra-latex/body)
    ("q" nil :color blue))

  (defhydra hydra-latex-preview (:color blue :hint nil)
    "
^Preview^          ^Clear^
_e_: environment   ^ ^
_b_: buffer        _C-b_: buffer
_d_: document      _C-d_: document
_p_: at point      _C-p_: at point
_r_: region        ^^
_s_: section       _C-s_: section

[_._]: back [_q_]: quit
"
    ("e" preview-environment)
    ("b" preview-buffer)
    ("C-b" preview-clearout-buffer)
    ("C-c" preview-clearout)
    ("d" preview-document)
    ("C-d" preview-clearout-document)
    ("p" preview-at-point)
    ("r" preview-region)
    ("C-p" preview-clearout-at-point)
    ("s" preview-section)
    ("C-s" preview-clearout-section)
    ("." hydra-latex/body)
    ("q" nil))
