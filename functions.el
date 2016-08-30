;; Personal Functions

;; By convention, I start them all with `sam--'
(defun sam--view-clipboard ()
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

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

(defun sam--comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun sam--switch-to-other-buffer ()
  "Switch to other buffer"
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun sam--fix-linum-size ()
  "Fixe la taille de charactère dans linum mode"
  (interactive)
  (set-face-attribute 'linum nil :height 80))

(defun sam--edit-init-file ()
  "Edit the emacs init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun sam--edit-keybindings ()
  (interactive)
  (find-file "~/dotfile/emacs/keybindings.el"))

(defun sam--edit-functions ()
  (interactive)
  (find-file "~/dotfile/emacs/functions.el"))

(defun sam--chrome-org-link ()
  (interactive)
  (insert (grab-mac-link 'chrome 'org)))

(defun sam--chrome-md-link ()
  (interactive)
  (insert (grab-mac-link 'chrome 'markdown)))

(defun sam--finder-md-link ()
  (interactive)
  (insert (grab-mac-link 'finder 'markdown)))

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

(defun sam--open-in-external-app ()
  "Open current file in external application."
  (interactive)
  (let ((file-path (if (eq major-mode 'dired-mode)
                       (dired-get-file-for-visit)
                     (buffer-file-name))))
    (if file-path
        (shell-command (format "open \"%s\"" file-path))
      (message "No file associated to this buffer."))))


;; from magnars ;; from spacemacs
(defun sam--delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
