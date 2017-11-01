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

(use-package org
  :load-path ("~/.emacs.d/private/org-mode")
  :commands (org-mode
             org-agenda-list
             org-capture
             org-store-link
             org-agenda)
  :mode (("\\.org\\'" . org-mode)
         ("*Org Agenda*" . org-agenda-mode)
         ("README\\'"   . org-mode))

  :bind*
  (:map dired-mode-map
   ("C-c C-l" . org-store-link))

  :config

  ;; ---------- Extension ---------------------------------------------------
  (use-package org-timeline :ensure t
    :disabled t)

  (use-package ox-tufte :ensure t :disabled t)

  (use-package org-bullets :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

    :config
    (setq org-bullets-bullet-list  '("➡" "➠" "➟" "➝" "↪")))

  (use-package org-indent :diminish "")

  (use-package orgit :ensure t :disabled t)

  (use-package ox-gfm :ensure t :disabled t)

  ;; ---------- BABEL -------------------------------------------------------
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((R . t)
                                 (perl . t)
                                 (python . t)
                                 ;; (clojure . t)
                                 (shell . t)
                                 (emacs-lisp . t)
                                 (dot . t)
                                 (makefile . t)
                                 ;; (js . t)
                                 ))

  (defun org-babel-tangle-all-block-same-file ()
    "tangle all blocks which belong to the same file."
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively #'org-babel-tangle)))

  (general-define-key
   :keymaps 'org-mode-map
   "s-e" 'org-babel-tangle-all-block-same-file
   "s-l" 'org-latex-export-to-latex)

  ;; ---------- default -----------------------------------------------------
  (require 'org-agenda)
  (require 'org-mu4e)

  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Org/TODO"
              "~/these/meta/nb/These.org"))

  (setq org-mu4e-link-query-in-headers-mode nil)
  (setq org-capture-use-agenda-date t) ; when press k from agenda, use agenda date.
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100) ; take advantage of the screen width
  (setq org-agenda-sticky nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-show-log t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-deadline-warning-days 4)
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  (setq org-agenda-time-grid
        '((daily today require-timed)
          "----------------"
          (800 1000 1200 1400 1600 1800)))
  ;; agenda start on mondays
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("c" "Simple agenda view"
           ((agenda "")
            (alltodo "")))))

;;; *** gtd with org

  (setq
   org-modules '(org-crypt)
   org-tags-column 80          ; aligne les tags très loin sur la droite
   org-hide-block-startup t    ; cache les blocks par défaut.
   org-refile-targets '(("~/these/meta/nb/These.org" :level . 2)
                        ("~/Org/TODO" :level . 2)
                        ("~/these/meta/nb/maybe.org" :level . 1)
                        ("~/Org/maybe.org" :level . 1))
   org-default-notes-file "~/Org/notes.org"
   org-capture-templates
   '(("t" "these - todo" entry (file+headline "~/these/meta/nb/These.org" "InBox") "** %?\n%U")
     ("r" "tickler"      entry (file+headline "~/these/meta/nb/These.org" "Tickler") "** %? %T")
     ("T" "todo"         entry (file+headline "~/Org/TODO" "Collecte") "** TODO %? %T")
     ("n" "notes"        entry (file+olp+datetree "~/Org/journal.org") "* %(hour-minute-timestamp) %?%^g\n")
     ("j" "lab"          entry (file+olp+datetree "~/these/meta/nb/journal.org") "* %(hour-minute-timestamp) %?%^g\n")))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; prevent an item to switch to completed if an item below it is not:
  (setq org-enforce-todo-dependencies t)
  (defun hour-minute-timestamp ()
    (format-time-string "%H:%M" (current-time)))

  (add-to-list 'org-modules 'org-mac-iCal)
  (setq org-agenda-include-diary nil)

;;;*** src block and babel

  (setq
   org-src-preserve-indentation t

;;;*** footnotes

   org-footnote-auto-adjust t
   org-footnote-define-inline nil
   org-footnote-fill-after-inline-note-extraction t
   org-footnote-section nil

;;;; *** export

   org-export-with-todo-keywords nil
   org-export-default-language "fr"
   org-export-backends '(ascii html icalendar latex md koma-letter)

;;;; latex

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
   ;; '(("AUTO" "inputenc" t) ("T1" "fontenc" t) ("" "graphicx" t) ("" "longtable" t) ("" "float" nil)
   ;;   ("" "hyperref" nil) ("" "wrapfig" nil) ("" "rotating" nil) ("normalem" "ulem" t)
   ;;   ("" "amsmath" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t)
   ;;   ("" "amssymb" t) ("scaled=0.9" "zi4" t) ("x11names, dvipsnames" "xcolor" t)
   ;;   ("protrusion=true, expansion=alltext, tracking=true, kerning=true" "microtype" t)
   ;;   ("" "siunitx" t) ("french" "babel" t))
   ;; extensions that listings packages in latex recognize.
   org-latex-listings-langs
   '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp") (c "C") (cc "C++") (fortran "fortran")
     (perl "Perl") (cperl "Perl") (python "Python") (ruby "Ruby") (html "HTML")
     (xml "XML") (tex "TeX") (latex "[LaTeX]TeX") (shell-script "bash") (gnuplot "Gnuplot")
     (ocaml "Caml") (caml "Caml") (sql "SQL") (sqlite "sql") (makefile "make")
     (R "r"))
   ;; files extensions that org considers as latex byproducts.
   org-latex-logfiles-extensions '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx"
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
   org-latex-tables-booktabs t
   org-startup-with-inline-images nil
   org-startup-indented t)

  (with-eval-after-load 'ox-latex
    (append-to-list
     'org-latex-classes
     '(("tant"
        "\\documentclass[twoside,a4paper,10pt]{tant}
\\addbibresource{reference.bib}
"
        ("\\part{%s}" . "\\part*{%s}")
        ("\\chapter{%s}" . "\\chapter*{%s}")
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

;;; * Keybindings

  (general-define-key
   :keymaps 'org-mode-map
   "C-c M-i" 'org-insert-link))
