(use-package org
  :load-path ("~/.emacs.d/private/org-mode")
  :commands (org-mode
             org-agenda-list
             org-capture
             org-store-link
             org-agenda)
  :mode (("\\.org\\'" . org-mode)
         ("*Org Agenda*" . org-agenda-mode))

  :bind*
  (:map dired-mode-map
   ("C-c C-l" . org-store-link))

  :config

  ;; ---------- Extension ---------------------------------------------------
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
    "s-l" 'org-latex-export-to-latex
    )

  ;; ---------- default -----------------------------------------------------
  (require 'org-agenda)

  ;; inspired from  http://pages.sachachua.com/.emacs.d/Sacha.html#orgce6f46d
  (setq org-agenda-files
        (list "~/Org/TODO"
              "~/these/meta/nb/TODO.org"))

  (setq org-capture-use-agenda-date t) ; when press k from agenda, use agenda date.
  (setq org-agenda-span 7)
  (setq org-agenda-tags-column -100)    ; take advantage of the screen width
  (setq org-agenda-sticky nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-use-tag-inheritance t)
  (setq org-agenda-show-log t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-deadline-if-done t)
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

  (org-add-link-type "ebib" 'ebib-open-org-link)

;;;*** gtd with org
  (setq
   org-modules '(org-crypt)
   org-tags-column 80                  ; aligne les tags très loin sur la droite
   org-hide-block-startup t            ; cache les blocks par défaut.
   org-refile-targets '(("~/Org/TODO" :level . 2)
                        ("~/stage/TODO" :level . 1)
                        ("~/Org/someday.org" :level . 1)
                        ("~/Org/knowledge.org" :level . 2))
   org-default-notes-file "~/Org/notes.org"
   org-capture-templates
   '(("t" "these - todo" entry (file+headline "~/these/meta/nb/TODO.org" "Thèse") "** TODO %? %T")
     ("e" "enseignements" entry (file+headline "~/these/meta/nb/TODO.org" "Enseignements") "** TODO %? %T")
     ("r" "réunion" entry (file+headline "~/these/meta/nb/TODO.org" "Réunion") "** %? %T")
     ("T" "todo" entry (file+headline "~/Org/TODO" "Collecte") "** TODO %? %T")
     ("n" "notes" entry (file+datetree "~/Org/journal.org") "* %(hour-minute-timestamp) %?\n")
     ("j" "lab-journal" entry (file+datetree "~/these/meta/nb/journal.org") "* %(hour-minute-timestamp) %?\n" )))

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
;;;*** export
   org-export-with-todo-keywords nil
   org-export-default-language "fr"
   org-export-backends '(ascii html icalendar latex md koma-letter)
;;;*** latex
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
   (quote (("AUTO" "inputenc" t) ("T1" "fontenc" t) ("" "graphicx" t) ("" "longtable" t) ("" "float" nil)
           ("" "hyperref" nil) ("" "wrapfig" nil) ("" "rotating" nil) ("normalem" "ulem" t)
           ("" "amsmath" t) ("" "textcomp" t) ("" "marvosym" t) ("" "wasysym" t)
           ("" "amssymb" t) ("scaled=0.9" "zi4" t) ("x11names, dvipsnames" "xcolor" t)
           ("protrusion=true, expansion=alltext, tracking=true, kerning=true" "microtype" t)
           ("" "siunitx" t) ("french" "babel" t)))
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
   org-startup-indented t)

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

  (defun org-agenda-cts ()
    (let ((args (get-text-property
                 (min (1- (point-max)) (point))
                 'org-last-args)))
      (nth 2 args)))

  (defhydra hydra-org-agenda-view (:hint nil :columns 1)
    "VIEW"
    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view
     (format "%s - day" (if (eq 'day (org-agenda-cts)) "[x]" "[ ]")))
    ("w" org-agenda-week-view
     (format "%s - week" (if (eq 'week (org-agenda-cts)) "[x]" "[ ]")))
    ("t" org-agenda-fortnight-view
     (format "%s - fortnight" (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]")))
    ("m" org-agenda-month-view
     (format "%s - month" (if (eq 'month (org-agenda-cts)) "[x]" "[ ]")))
    ("y" org-agenda-year-view
     (format "%s - year" (if (eq 'year (org-agenda-cts)) "[x]" "[ ]")))
    ("q" (message "Abort") :exit t))

  (bind-key (kbd "v") #'hydra-org-agenda-view/body org-agenda-mode-map)


;;;* Keybindings
  (general-define-key
   :keymaps 'org-mode-map
    (general-chord ",c") 'org-shiftcontrolleft
    (general-chord ",t") 'org-shiftcontroldown
    (general-chord ",s") 'org-shiftcontrolup
    (general-chord ",r") 'org-shiftcontrolright
    (general-chord ";C") 'org-metaleft
    (general-chord ";T") 'org-metadown
    (general-chord ";S") 'org-metaup
    (general-chord ";R") 'org-metaright))

;;;* Keybindings


;; TODO j'ai eu l'idée d'un snippet qui permettrait de splitter les
;; chunks en deux. au moment où je dois choisir le file to tangle in,
;; la fonction parserai le script pour construire une liste de tous
;; les :tangle file, de façon à pouvoir choisir le fichier dans lequel
;; je veux que ça aille. par défault, il devrait choisir le file dans
;; le chunk juste au dessus.
