(use-package org
  :ensure org-plus-contrib
  :commands (org-mode
	     org-agenda-list
	     org-store-link
	     )

  :bind*
  (:map dired-mode-map
   ("C-c C-l" . org-store-link))

  :config

  (use-package org-bullets :ensure t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

    :config
    (setq org-bullets-bullet-list  '("➡" "➠" "➟" "➝" "↪"))
    )

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((R . t)
                                 (python . t)
                                 (clojure . t)
                                 (sh . t)
                                 (emacs-lisp . t)
                                 (dot . t)
                                 (js . t)))

  (require 'org-agenda)
  (setq
   org-modules '(org-crypt)
;;; gtd with org
   org-tags-column 80		       ; aligne les tags très loin sur la droite
   org-hide-block-startup t	       ; cache les blocks par défaut.
   org-refile-targets '(("~/Org/TODO" :level . 2)
                        ("~/stage/TODO" :level . 1)
                        ("~/Org/someday.org" :level . 1)
                        ("~/Org/knowledge.org" :level . 2))
   org-default-notes-file "~/Org/notes.org"
   org-capture-templates
   '(("c" "collecte" entry (file+headline "~/Org/TODO" "Collecte") "** TODO %? %^G\n%U \n%i")
     ("s" "stage" entry (file+headline "~/stage/TODO" "capture")   "** TODO %? %^G\n%U \n%i")
     ("j" "journal" entry (file+datetree "~/Org/journal.org")      "* %?\nAjouté le %U\n %i\n  %a")
     ("n" "notes" entry (file+headline "~/Org/notes.org" "Notes")  "** %U  %^g\n%?")
     ("J" "lab-journal" entry (file+datetree "~/stage/notes/journal.org") "* %?\nAjouté le %U\n %i\n %a"))
;;; src block and babel
   org-src-preserve-indentation t
;;; footnotes
   org-footnote-auto-adjust t
   org-footnote-define-inline t
   org-footnote-fill-after-inline-note-extraction t
   org-footnote-section nil
;;; export
   org-export-with-todo-keywords nil
   org-export-default-language "fr"
   org-export-backends '(ascii html icalendar latex md koma-letter)
;;; latex
   ;; moyen d'export latex
   org-latex-pdf-process
   (list "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "bibtex %f"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
   org-latex-image-default-width "1\\linewidth"
   org-highlight-latex-and-related '(latex entities) ; colore les macro LaTeX
   ;; tufte-handout class by default.
   org-latex-default-class "tufte-handout"
   ;; default package list with sensible options
   org-latex-default-packages-alist
   (quote (("AUTO" "inputenc" t)
           ("T1" "fontenc" t)
           ("" "graphicx" t)
           ("" "longtable" t)
           ("" "float" nil)
           ("" "hyperref" nil)
           ("" "wrapfig" nil)
           ("" "rotating" nil)
           ("normalem" "ulem" t)
           ("" "amsmath" t)
           ("" "textcomp" t)
           ("" "marvosym" t)
           ("" "wasysym" t)
           ("" "amssymb" t)
           ("scaled=0.9" "zi4" t)
           ("x11names, dvipsnames" "xcolor" t)
           ("protrusion=true, expansion=alltext, tracking=true, kerning=true" "microtype" t)
           ("" "siunitx" t)
           ("french" "babel" t)))
   ;; extensions that listings packages in latex recognize.
   org-latex-listings-langs '((emacs-lisp "Lisp")
                              (lisp "Lisp")
                              (clojure "Lisp")
                              (c "C")
                              (cc "C++")
                              (fortran "fortran")
                              (perl "Perl")
                              (cperl "Perl")
                              (python "Python")
                              (ruby "Ruby")
                              (html "HTML")
                              (xml "XML")
                              (tex "TeX")
                              (latex "[LaTeX]TeX")
                              (shell-script "bash")
                              (gnuplot "Gnuplot")
                              (ocaml "Caml")
                              (caml "Caml")
                              (sql "SQL")
                              (sqlite "sql")
                              (makefile "make")
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
   org-startup-indented t
   )

  (use-package org-indent
    :diminish "")

  ;; this function is used to append multiple elements to the list 'ox-latex
  (defun append-to-list (list-var elements)
    "Append ELEMENTS to the end of LIST-VAR. The return value is the new value of LIST-VAR."
    (unless (consp elements) (error "ELEMENTS must be a list"))
    (let ((list (symbol-value list-var)))
      (if list
          (setcdr (last list) elements)
        (set list-var elements)))
    (symbol-value list-var))

  (with-eval-after-load 'ox-latex
    (append-to-list
     'org-latex-classes
     '(("tufte-book"
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

  (defun org-insert-heading-with-date-after-current ()
    "Insert a new heading with current date same level as current,
     after current subtree."
    (interactive)
    (org-back-to-heading)
    (org-insert-heading)
    (insert-timestamp)
    (org-move-subtree-down)
    (end-of-line 1))

  )

(use-package org-journal :ensure t
  :commands (org-journal-new-entry)

  :general
  (general-define-key
   :states '(normal)
   :prefix "SPC"
   :non-normal-prefix " "
   "jn" 'org-journal-new-entry
   )

  :config
  (setq org-journal-dir "~/Org/journal")
  )
