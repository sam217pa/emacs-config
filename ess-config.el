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

;;; Code

(use-package ess-site
  :ensure ess
  :mode
  (("\\.sp\\'"           . S-mode)
   ("/R/.*\\.q\\'"       . R-mode)
   ("\\.[qsS]\\'"        . S-mode)
   ("\\.ssc\\'"          . S-mode)
   ("\\.SSC\\'"          . S-mode)
   ("\\.[rR]\\'"         . R-mode)
   ("\\.[rR]nw\\'"       . Rnw-mode)
   ("\\.[sS]nw\\'"       . Snw-mode)
   ("\\.[rR]profile\\'"  . R-mode)
   ("NAMESPACE\\'"       . R-mode)
   ("CITATION\\'"        . R-mode)
   ("\\.omg\\'"          . omegahat-mode)
   ("\\.hat\\'"          . omegahat-mode)
   ("\\.lsp\\'"          . XLS-mode)
   ("\\.do\\'"           . STA-mode)
   ("\\.ado\\'"          . STA-mode)
   ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
   ("\\.jl\\'"           . ess-julia-mode)
   ("\\.[Ss]t\\'"        . S-transcript-mode)
   ("\\.Sout"            . S-transcript-mode)
   ("\\.[Rr]out"         . R-transcript-mode)
   ("\\.Rd\\'"           . Rd-mode)
   ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
   ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands (R
             stata
             julia
             SAS)

  :init
  (add-hook! 'ess-mode-hook
    (smartparens-mode 1)
    (lesspy-mode 1)
    (run-hooks 'prog-mode-hook 'company-mode-hook))

  (add-hook! 'inferior-ess-mode-hook
    (setq-local outline-regexp "^>")
    (rainbow-mode t)
    ;; do not truncate line in the R repl:
    (toggle-truncate-lines 1))

  (setq ess-offset-continued 2          ; offset after first statement
        ess-expression-offset 2         ; offset for expression
        ess-nuke-trailing-whitespace-p t ;delete trailing whitespace
        ess-default-style 'RStudio) ; set default style for R source file
  (setq ess-indent-with-fancy-comments nil)
  (setq ess-swv-plug-into-AUCTeX-p t)
  (setq ess-eval-visibly 'nowait)
  (setq ess-roxy-insert-prefix-on-newline t)
  (setq ess-eldoc-show-on-symbol t)
  ;; ess should use default completing-read, either ivy or helm.
  (setq ess-use-ido nil)
  (setq ess-use-flymake nil) ; flymake does not work with ess right now.
  (setq ess-r-package-auto-enable-namespaced-evaluation t)

  :config
  (ess-toggle-underscore nil)

  (sp-local-pair 'ess-mode "{" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'ess-mode "(" nil
                 :post-handlers '((sam--create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'ess-mode "%" "%")

;;;; keybindings

  (general-define-key
   :keymaps 'ess-mode-map
   "RET" 'ess-newline-and-indent
   "C-RET" 'ess-eval-region-or-line
   "M-RET" 'ess-eval-function-or-paragraph
   "M-p"   'sp-backward-up-sexp
   "M-n"   'sp-up-sexp
   " " 'ess-insert-S-assign             ; shift alt space
   " " 'ess-insert-S-assign             ; shift space
   ))

(use-package ess-rutils
  :after ess-site
  :config
  (ess-rutils-mode)
  (setq ess-rutils-keys t))
