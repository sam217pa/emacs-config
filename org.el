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

(use-package org
  :defer t
  :ensure org-plus-contrib
  :commands (org-mode
             org-capture
             org-store-link)
  :mode (("\\.org\\'" . org-mode)
         ("README\\'"   . org-mode))

  :bind*
  (("C-c C-w" . org-refile)
   ("C-c C-l" . org-store-link)
   ("C-M-c" . org-capture))

  :config


;;;; BABEL

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle)))

;;;;  attachment system

  (setq org-attach-method 'lns)    ; make symlink instead of hard copy
  (setq org-attach-archive-delete t) ; delete attachment when archiving entry
  (setq org-attach-directory ".attach/") ; change folder from data/ to .attach/

;;;; gtd with org

  (setq
   org-modules '(org-crypt)
   org-tags-column 80        ; aligne les tags très loin sur la droite
   org-hide-block-startup t  ; cache les blocks par défaut.
   org-refile-targets '(("~/these/meta/nb/These.org" :level . 2)
                        ("~/Org/TODO" :level . 2)
                        ("~/Org/TODO" :level . 1)
                        ("~/these/meta/nb/maybe.org" :level . 1)
                        ("~/Org/maybe.org" :level . 1))
   org-default-notes-file "~/Org/notes.org")

  (setq
   org-capture-templates
   '(("t" "these - Todo"     entry (file+headline "~/these/meta/nb/These.org" "InBox"             ) "** %?\n%U")
     ("l" "these - Lab"      entry (file+olp+datetree "~/these/meta/nb/journal.org"               ) "* %(hour-minute-timestamp) %?%^g\n")
     ("r" "these - tickleR"  entry (file+headline "~/these/meta/nb/These.org" "Tickler"           ) "** %?\n%T")
     ("a" "these - rapport"  entry (file+headline "~/these/these/notes-manuscrit.org" "Collecte"  ) "** %?\n%U")
     ("c" "perso - Collecte" entry (file+headline "~/Org/TODO" "Collecte"                         ) "** %?\n%U\n")
     ("n" "perso - Notes"    entry (file+olp+datetree "~/Org/journal.org"                         ) "* %(hour-minute-timestamp) %?%^g\n")))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; prevent an item to switch to completed if an item below it is not:
  (setq org-enforce-todo-dependencies t)
  (defun hour-minute-timestamp ()
    (format-time-string "%H:%M" (current-time)))

  (add-to-list 'org-modules 'org-mac-iCal)

  ;; (setq org-link-abbrev-alist nil)
  (append-to-list!
   'org-link-abbrev-alist
   '(("wiki" . "https://en.wikipedia.org/wiki/%h")
     ("gsc"  . "https://scholar.google.com/scholar?q=%h")
     ("sep"  . "https://plato.stanford.edu/search/search?query=%h")
     ("etym" . "http://www.cnrtl.fr/etymologie/%h")
     ("bu"   . "http://hola.univ-lyon1.fr/ipac20/ipac.jsp?menu=search&aspect=basic_search&npp=10&ipp=20&spp=20&profile=scd&ri=&index=.GK&term=%h&terms=&valider=Ok")))

  (setq org-crypt-disable-auto-save t)

;;;; src block and babel

  (setq
   org-src-preserve-indentation t

;;;; footnotes

   org-footnote-auto-adjust t
   org-footnote-define-inline nil
   org-footnote-fill-after-inline-note-extraction t
   org-footnote-section nil

;;;; export

   org-export-with-todo-keywords nil
   org-export-default-language "fr"
   org-export-backends '(ascii html icalendar latex md koma-letter))
  (setq org-export-with-tags nil)


  (setq org-startup-with-inline-images t)
  (setq org-startup-indented t)

  ;; rescale images to 400px if no with attribute is set (see
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg01402.html)
  (setq org-image-actual-width '(400))

;;; Keybindings

  (general-define-key
   :keymaps 'org-mode-map
   "s-e" 'org-babel-tangle-all-block-same-file
   "s-l" 'org-latex-export-to-latex
   "C-c ." 'org-time-stamp
   "C-c M-i" 'org-insert-link
   ;; mnémotechnique : iMage (i is for imenu already)
   "C-c m" 'hydra-org-image/body
   "C-c $" 'hydra-org-archive/body)

  (general-define-key
   :keymaps 'org-mode-map
   :prefix "C-c e"
   "d" 'org-decrypt-entry
   "e" 'org-encrypt-entry
   "s" 'org-sparse-tree
   "t" 'org-tags-sparse-tree)

  (defhydra hydra-org-archive (:color red :columns 1)
    ("a" org-archive-subtree "archive")
    ("n" org-next-visible-heading "next")
    ("t" org-next-visible-heading "next")
    ("p" org-previous-visible-heading "previous")
    ("s" org-previous-visible-heading "previous"))

  (defhydra hydra-org-image (:color red :hint nil)
    "
Display inline images ?
_y_es  _n_o    _t_oggle
"
    ("y" org-display-inline-images)
    ("n" org-remove-inline-images)
    ("t" org-toggle-inline-images)))

;;; Extensions

(use-package org-agenda
  :ensure org-plus-contrib
  :bind* (("C-c a" . org-agenda))
  :config

  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Org/TODO"
              "~/these/meta/nb/These.org"))

  (setq org-capture-use-agenda-date t) ; when press k from agenda, use agenda date.
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100) ; take advantage of the screen width
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-deadline-warning-days 4)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  ;; agenda start on mondays
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))
          ("l" "Lab" tags-todo "@these"
           ((org-agenda-files '("~/these/meta/nb/These.org"))
            (org-agenda-sorting-strategy '(timestamp-up priority-up)))
           ("~/these/meta/nb/These.html"))
          ("p" "perso" tags "@perso"
           ((org-agenda-sorting-strategy '(ts-up priority-up))))))

  (setq org-agenda-include-diary nil))

(use-package ob-R
  :ensure org-plus-contrib
  :commands (org-babel-execute:R))

(use-package ob-perl
  :ensure org-plus-contrib
  :commands (org-babel-execute:perl))

(use-package ob-python
  :ensure org-plus-contrib
  :commands (org-babel-execute:python))

(use-package ob-shell
  :ensure org-plus-contrib
  :commands (org-babel-execute:shell))

(use-package ob-emacs-lisp
  :ensure org-plus-contrib
  :commands (org-babel-execute:emacs-lisp))

(use-package ob-dot
  :ensure org-plus-contrib
  :commands (org-babel-execute:dot))

(use-package ob-makefile
  :ensure org-plus-contrib
  :commands (org-babel-execute:makefile))

(use-package ox-latex
  :ensure org-plus-contrib
  :commands (org-latex-export-as-latex
             org-latex-export-to-latex)
  :config

  (setq org-latex-listings 'minted)
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "color"))
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (setq
   ;; moyen d'export latex
   org-latex-pdf-process
   (list "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "biber %f"
         "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   org-latex-image-default-width "1\\linewidth"
   org-highlight-latex-and-related '(latex entities) ; colore les macro LaTeX
   ;; tufte-handout class by default.
   org-latex-default-class "tant"
   ;; default package list with sensible options
   org-latex-default-packages-alist nil
   org-latex-listings-langs
   '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran")
     (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML")
     (xml "XML") (tex "TeX") (latex "[LaTeX]TeX") (shell-script "bash") (gnuplot "Gnuplot")
     (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (makefile "make")
     (R "r"))
   ;; files extensions that org considers as latex byproducts.
   org-latex-logfiles-extensions
   '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx"
     "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl")
   org-latex-minted-langs '((emacs-lisp "common-lisp")
                            (cc "c++")
                            (cperl "perl")
                            (shell-script "bash")
                            (caml "ocaml")
                            (python "python")
                            (ess "R"))
   org-latex-remove-logfiles t
   org-src-fontify-natively t
   org-latex-table-caption-above nil
   org-latex-tables-booktabs t)

  (append-to-list!
   'org-latex-classes
   '(("tant"
      "\\documentclass[twoside,a4paper,10pt]{tant}
% \\addbibresource{reference.bib}
"
      ;; ("\\part{%s}" . "\\part*{%s}")
      ;; ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-book"
      "\\documentclass[a4paper, sfsidenotes, justified, notitlepage]{tufte-book}
       \\input{/Users/samuelbarreto/.templates/tufte-book.tex}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("tufte-handout"
      "\\documentclass[a4paper, justified]{tufte-handout}
       \\input{/Users/samuelbarreto/.templates/tufte-handout.tex}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}"))
     ("rapport" "\\documentclass[11pt, oneside]{scrartcl}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass[presentation]{beamer}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("journal"
      "\\documentclass[9pt, oneside, twocolumn]{scrartcl}
       \\input{/Users/samuelbarreto/.templates/journal.tex}"
      ("\\part{%s}" . "\\section*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")))))

(use-package ox-twbs
  :ensure t
  :after org
  :commands (org-twbs-publish-to-html))

(use-package ox-tufte
  :ensure t
  :after org
  :commands (org-tufte-export-to-file))

(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("➡" "➠" "➟" "➝" "↪")))

(use-package org-indent
  :ensure org-plus-contrib
  :diminish ""
  :after org)

(use-package ox-pandoc
  :ensure t
  :after org
  :commands (org-pandoc-export))

(use-package ox-gfm
  :ensure t
  :after org
  :commands (org-gfm-export-to-markdown))

(use-package org-download
  :ensure t
  :after org
  :config

  (defvar sam|org-download-dir "./img/"
    "Default folder to place `org-download' captures in.")

  (defun sam|img-dir ()
    (let (target sam|org-download-dir)
      (cond ((file-directory-p target) target)
            (t (make-directory target) target))))

  (setq-default org-download-heading-lvl nil)
  (setq-default org-download-image-dir sam|org-download-dir)
  (when (eq system-type 'darwin)
    (setq-default org-download-screenshot-method "screencapture -i %s"))

  (general-define-key
   :keymaps 'org-mode-map
   :prefix "C-c y"
   "e" 'org-download-edit
   "i" 'org-download-image
   "s" 'org-download-screenshot
   "y" 'org-download-yank
   "k" 'org-download-delete))

(use-package org-web-tools
  :ensure t
  :commands (org-web-tools-insert-link-for-url
             org-web-tools-insert-web-page-as-entry
             org-web-tools-read-url-as-org
             org-web-tools-convert-links-to-page-entries))
