;;; Personal Functions
;;

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

(defun sam--revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(defun sam--insert-timestamp ()
  "Insert a quick timestamp."
  (interactive)
  (insert (format "%s" (format-time-string "%Y-%m-%d" (current-time)))))

(defun sam--duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(defun sam|comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun sam|switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun sam--eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
Requires smartparens because all movement is done using
`sp-up-sexp'. An optional ARG can be used which is passed to
`sp-up-sexp' to move out of more than one sexp."
  (interactive "p")
  (require 'smartparens)
  (save-excursion
    (let ((max 10))
      (while (and (> max 0)
                  (sp-point-in-string-or-comment))
        (decf max)
        (sp-up-sexp)))
    (sp-up-sexp arg)
    (call-interactively 'eval-last-sexp)))

;; from https://github.com/syl20bnr/spacemacs/
(defun sam|open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (shell-command (format "open \"%s\"" file-path))
      (message "No file associated to this buffer."))))


;;; Blog related functions
;; from https://blog.tohojo.dk/2015/10/integrating-hugo-into-emacs.html

(setq hugo-base-dir "~/blog/samuel-blog/"
      hugo-buffer "*hugo*")

(defun hugo-new-post ()
  "This function asks for the title of the new post, generates a
filename for it by lower-casing the title and replacing all
special characters with hyphens, then runs hugo new to create the
posting. Finally, the file is opened and the correct title is
inserted in place of the one Hugo generates from the file name."
  (interactive)
  (let* ((title (read-from-minibuffer "Title: "))
         (filename
          (concat "post/"
                  (read-from-minibuffer
                   "Filename: "
                   (replace-regexp-in-string
                    "-\\.md" ".md"
                    (concat (downcase (replace-regexp-in-string "[^a-z0-9]+" "-" title)) ".md")))))
         (path (concat hugo-base-dir "content/" filename)))
    (if (file-exists-p path)
        (message "File already exists!")
      (hugo-command "new" filename)
      (find-file path)
      (hugo-replace-key "title" title)
      (goto-char (point-max))
      (save-buffer))))

(defun hugo-command (&rest args)
  (let ((default-directory (expand-file-name hugo-base-dir)))
    (apply 'call-process "hugo" nil hugo-buffer t args)))

(defun hugo-replace-key (key val)
  (save-excursion
    (goto-char (point-min))
                                        ; quoted value
    (if (and (re-search-forward (concat key " = \"") nil t)
             (re-search-forward "[^\"]+" (line-end-position) t))
        (or (replace-match val) t) ; ensure we return t
                                        ; unquoted value
      (when (and (re-search-forward (concat key " = ") nil t)
                 (re-search-forward ".+" (line-end-position) t))
        (or (replace-match val) t)))))

(defun hugo-server (&optional arg)
  "This simply spawn the Hugo server process and attaches it to
the defined Hugo buffer – or kills it if it is already
running. The server is run with the --buildDrafts --watch -d dev
command line options, which causes draft posts to also be built,
turns on watch mode, and uses a separate directory for the
compiled files (so as not to interfere with publishing). If run
without a prefix argument, this function will also open a browser
pointing to the Hugo localhost server."
  (interactive "P")
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/"))
         (proc (get-buffer-process hugo-buffer)))
    (if (and proc (process-live-p proc))
        (progn (interrupt-process proc)
               (message "Stopped Hugo server"))
      (start-process "hugo" hugo-buffer "hugo" "server" "--buildDrafts" "--watch" "-d" "dev")
      (message "Started Hugo server")
      (unless arg
        (browse-url "http://localhost:1313/")))))

(defun hugo-publish ()
  (interactive)
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/")))
    (when (call-process "bash" nil hugo-buffer t  "./upload.sh")
      (message "Blog published"))))

(defun hugo-undraft ()
  (interactive)
  (when (and (hugo-replace-key "date" (iso-timestamp))
             (hugo-replace-key "draft" "false"))
    (save-buffer)
    (message "Removed draft status and updated timestamp")))

(defun iso-timestamp ()
  (concat (format-time-string "%Y-%m-%dT%T")
          ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
           (format-time-string "%z"))))


;;;; correct indentation with keywords
;;
;; A redefinition of lisp-indent-function to make it respect sexp that start with a keyword
;; (:keymap patate
;;          pamplemousse)
;; is the default behavior while
;; (:keymap patate
;;  pamplemousse)
;; is the expected behavior
;; see (https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94)

(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(defun sam|iterm-here ()
  "Go to present working dir and focus iterm"
  (interactive)
  (let ((dir (shell-quote-argument (expand-file-name default-directory))))
    (do-applescript
     (concat
      " tell application \"iTerm2\"\n"
      "   tell the current session of current window\n"
      (format "     write text \"cd %s\" \n"
              ;; string escaping madness for applescript
              (replace-regexp-in-string "\\\\" "\\\\\\\\" dir))
      "   end tell\n"
      " end tell\n"
      " do shell script \"open -a iTerm\"\n"))))

(defun sam|iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun sam|finder-here ()
  (interactive)
  (let* ((dir default-directory)
         (scr (format " do shell script \"open %s\"\n" dir)))
    (do-applescript scr)))

;; adapted from
;; http://emacs.stackexchange.com/questions/202/close-all-dired-buffers
(defun kill-diff-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (member (buffer-local-value 'major-mode buffer)
                        '(diff-mode magit-diff-mode magit-process-mode))
            (kill-buffer buffer)))
        (buffer-list)))

;; syntax highlight hugo src block
(font-lock-add-keywords
 'markdown-mode
 '(("src\\|{{< figure\\|caption\\|link\\|>}}" . font-lock-keyword-face)))

(defvar counsel-colors--solarized-alist
  '(("brblack"   . "#002b36")
    ("black"     . "#073642")
    ("brgreen"   . "#586e75")
    ("bryellow"  . "#657b83")
    ("brblue"    . "#839496")
    ("brcyan"    . "#93a1a1")
    ("white"     . "#eee8d5")
    ("brwhite"   . "#fdf6e3")
    ("yellow"    . "#b58900")
    ("brred"     . "#cb4b16")
    ("red"       . "#dc322f")
    ("magenta"   . "#d33682")
    ("brmagenta" . "#6c71c4")
    ("blue"      . "#268bd2")
    ("cyan"      . "#2aa198")
    ("green"     . "#859900"))
  "This a list of colors defined by the Solarized color
  palette.")


(defun counsel-colors-solarized ()
  "Show a list of all solarized colors.

You can insert or kill the name or the hexadecimal rgb value of the
selected candidate."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "%d Solarized color: "
              (mapcar (lambda (x)
                        (concat
                         (propertize
                          (format "%-25s" (car x)))
                         (propertize
                          (format "%8s  " (cdr x))
                          'face (list :foreground (car x)))
                         (propertize
                          (format "%10s" " ")
                          'face (list :background (cdr x)))))
                      counsel-colors--solarized-alist)
              :require-match t
              :action (lambda (x) (insert (substring-no-properties x 26 33)))
              :caller 'counsel-colors-solarized
              :sort t)))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun sam|unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; this function is used to append multiple elements to the list 'ox-latex
(defun append-to-list! (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR. The return value is the new value of LIST-VAR."
  (unless (consp elements) (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))


;;; lab notebook
(setq journal-base-dir "~/these/meta/journal/")

(defun journal-command (&rest args)
  (let ((default-directory (expand-file-name journal-base-dir)))
    (apply 'call-process "hugo" nil hugo-buffer t args)))

(defun journal-post ()
  (interactive)
  (let* ((filename
          (concat "post/"
                  (format-time-string "%Y-%m-%d" (current-time))
                  ".md"))
         (path (concat journal-base-dir "content/" filename)))
    (if (file-exists-p path)
        (find-file path)
      (journal-command "new" filename)
      (find-file path)
      (goto-char (point-min))
      (save-buffer))))

(defun sam|delete-to-sentence-beg ()
  (interactive)
  (save-excursion
    (let ((end (point)))
      (backward-sentence)
      (delete-region (point) end))))

;; from http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(defun sam--simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))

(advice-add 'pop-to-mark-command :around
            #'modi/multi-pop-to-mark)

(setq set-mark-command-repeat-pop t)

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((looking-at outline-regexp)
         (ignore-errors (outline-narrow-to-subtree)))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(defun mark-line ()
  "Mark the whole line"
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (next-line)
  (setq deactivate-mark nil))

(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
    (lambda ()
      (interactive)
      (ignore-errors
        (funcall fn)))))

(defun kill-word-ap (arg)
  (interactive "P")
  (let* ((argp (and arg (= 4 (prefix-numeric-value arg))))
         (beg (beginning-of-thing (if argp 'symbol 'word)))
         (end (end-of-thing (if argp 'symbol 'word))))
    (save-excursion
      (kill-region beg end))))

;; from  http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun sam/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'sam/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun sam|set-transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

;; from http://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; from https://www.emacswiki.org/emacs/ExecuteExternalCommand
(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(defalias 'kill-frame #'delete-frame)

(custom-set-variables '(epg-gpg-program  "gpg2"))

;;; counsel font

(defun counsel-font ()
  "Change font of current frame"
  (interactive)
  (ivy-read "Chose font :"
            (font-family-list)
            :caller 'counsel-font
            :action (lambda (x) (set-frame-font x))))

;;; counsel accession numbers

(defun sam--completion-collection (col)
  (mapcar (lambda (x)
            (concat (propertize (car x) 'font-lock-face '(:foreground "#268bd2"))
                    " => "
                    (propertize (cadr x) 'face 'slanted)))
          col))

(defun sam--completion-collection-out (candidate)
  (substring-no-properties candidate 0 (string-match " => " candidate)))

(defun sam--genome-accession-numbers-action (candidate)
  (insert (sam--completion-collection-out candidate)))

(defun sam|genome-accession-numbers ()
  (interactive)
  (let* ((accessions '(("NC_005966.1" "Acinetobacter baylyi ADP1")))
         (cols (sam--completion-collection accessions)))
    (ivy-read "Chose genome ?" cols
              :action #'sam--genome-accession-numbers-action)))

;;; use-package jump

(defun use-package-jump--list-calls ()
  (let ((packages))
    (save-excursion
      (goto-char (point-max))
      (while (beginning-of-defun)
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (when (string-match "^(use-package \\([^[:space:]\n]+\\)" line)
            (push (cons (match-string-no-properties 1 line) (point))
                  packages)))))
    packages))

(defun use-package-jump ()
  "Jump to an outer-level `use-package' definition in current buffer."
  (interactive)
  (let ((packages (use-package-jump--list-calls)))
    (goto-char (cdr (assoc (ivy-completing-read "Package: " packages)
                           packages)))))

(defun helm--goto-use-package-call (candidate)
  (push-mark)
  (goto-char candidate))

(defun helm-use-package-jump ()
  "Jump to an outer-level `use-package' definition in current buffer."
  (interactive)
  (helm
   :sources (helm-build-sync-source "Packages"
              :candidates (use-package-jump--list-calls)
              :fuzzy-match t
              :action (helm-make-actions
                       "Go to use-package call" #'helm--goto-use-package-call))
   :buffer "*helm upj*"))

;;;; Hidden mode line mode

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

(defun sam|indent-region ()
  "Indent region "
  (interactive)
  (let ((beg (region-beginning))
        (end (region-end)))
    (indent-region beg end)))

(defun sam|indent-paragraph ()
  "Indent paragraph at point according to mode"
  (interactive)
  (save-excursion
    (mark-paragraph)
    (indent-region (region-beginning) (region-end))))

(defun sam|join-to-next-line ()
  "Join current line to next line."
  (interactive)
  (join-line 4))


;;; Compile on save mode

(defun compile-on-save-start ()
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
  (if compile-on-save-mode
      (progn  (make-local-variable 'after-save-hook)
              (add-hook 'after-save-hook 'compile-on-save-start nil t))
    (kill-local-variable 'after-save-hook)))

;;; * Side Window Management

(defun sam|maximize-window ()
  "Maximize frame on first use, toggle frame fullscreen on second
consecutive use."
  (interactive)
  (let* ((second? (eq last-command this-command))
         (fullscreen (frame-parameter nil 'fullscreen))
         (maximized? (eq 'maximized fullscreen))
         (fullscreen? (eq 'fullboth fullscreen)))
    (cond ((and second? maximized?)
           (toggle-frame-fullscreen))
          (fullscreen?
           (toggle-frame-fullscreen))
          (t
           (toggle-frame-maximized)))))

(defun sam|main-window (&optional frame)
  "Refocus the main editing window.

Delete all side windows at first use ; at second consecutive use
it also delete other normal windows currently active in the
frame."
  (interactive)
  (let* ((frame (window-normalize-frame frame))
         (window--sides-inhibit-check t)
         (sw? (window-with-parameter 'window-side nil frame)))
    (cond ((and (eq last-command this-command) sw?)
           (ignore-errors (window-toggle-side-windows))
           (delete-other-windows))
          (sw?
           (window-toggle-side-windows))
          (t
           (delete-other-windows)))))

;;; Thesis project

(defun sam|thesis-projects ()
  "Switch to a data projects in my thesis folder"
  (interactive)
  (let* ((cmd "find ~/these/data -d 1 -type d")
         (dirs (split-string (shell-command-to-string cmd) "\n" t))
         (projects (seq-map #'file-name-nondirectory dirs))
         (candidates (seq-mapn #'cons projects dirs)))
    (ivy-read "Chose data folder :" candidates
              :action (lambda (_) (find-file (cdr _)))
              :caller #'sam|thesis-projects)))

;;; Snippet helper

(defun sam--comment-date ()
  (let ((time (format-time-string "[%Y-%m-%d %H:%M:%S]")))
    (format "%s %s\n%s\t" comment-start time comment-start)))

(defun sam--efetch-formats ()
  (let* ((options '(("fasta" "Fasta Sequence")
                    ("genbank" "GenBank Resume")
                    ("gbwithparts" "GenBank with sequence")))
         (col (sam--completion-collection options)))
    (sam--completion-collection-out
     (ivy-read "Choose format :" col))))

(defun sam--export-code ()
  (let* ((options '(("code" "Code only")
                    ("results" "Results only")
                    ("both" "Code and Results")
                    ("none" "None")))
         (col (sam--completion-collection options)))
    (sam--completion-collection-out
     (ivy-read "Choose format :" col))))

(defun org-enquote! (beg end)
  (interactive "r")
  (org--wrap "QUOTE" beg end))

(defun org-ensrc! (beg end)
  (interactive "r")
  (org--wrap "SRC" beg end))

(defun org--wrap (block-name beg end)
  (let ((beg-name (format "#+BEGIN_%s\n" (upcase block-name)))
        (end-name (format "#+END_%s\n" (upcase block-name))))
    (save-excursion
      (goto-char end)
      (if (= end (point-at-bol))
          (insert end-name)
        (insert (concat "\n" end-name))))
    (save-excursion
      (goto-char beg)
      (if (= beg (point-at-bol))
          (insert beg-name)
        (insert (concat "\n" beg-name))))))
