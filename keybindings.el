;; Most of the configuration should be done under general.el
;; Most of the keybindings should have a clear and precise prefix.
;; common functions should be under the same prefix (leader)
;;
;; Emacs related functions goes to the prefix `e'.
;; Major-mode related functions goes to the prefix `,'.


(use-package general :ensure t
  :config

  (general-evil-setup t)

  ;; This chunks contains all the keybindings that I use regularly.
  ;; They are placed under the prefix SPC, as in spacemacs.
  ;; Absolutely all functions that I use must be referenced here.
  ;; There is another leader via C-SPC which gives shorter keybindings
  ;; for stuff that I use more often.
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix " "

   ;; simple command
   "/"   'counsel-ag
   "TAB" '(sam--switch-to-other-buffer :which-key "prev buffer")
   "SPC" '(avy-goto-word-or-subword-1  :which-key "go to char")

   ;; Applications
   "a" '(:ignore t :which-key "Applications")
   "ar" 'ranger

   ;; Buffer
   "b" '(:ignore t :which-key "Buffer")
   "bb"  '(ivy-switch-buffer :which-key "switch buffer")
   "bd"  '(kill-buffer-and-window :which-key "delete buffer")

   ;; Comment or Compile
   "c" '(:ignore t :which-key "Comment")
   "cl"  '(sam--comment-or-uncomment-region-or-line :which-key "comment line")

   ;; Window management
   "é" '(:ignore t :which-key "Window")
   "éc"  'other-window
   "éd"  'ace-delete-window
   "ér"  'other-window
   "éé"  'hydra-window/body
   "éh"  '(split-window-vertically :which-key "split horizontal")
   "ém"  '(delete-other-windows :which-key "maximize current")
   "év"  '(split-window-horizontally :which-key "split vertical")
   "é|"  '(split-window-horizontally :which-key "split vertical")
   "é-"  '(split-window-vertically :which-key "split horizontal")

   ;; Find and Files
   "f" '(:ignore t :which-key "Files")
   "fd"  '(counsel-git :which-key "find in git dir")
   "fD"  '(sam--delete-current-buffer-file :which-key "delete file")
   "fe" '(:ignore t :which-key "edit")
   "fei" '(sam--edit-init-file :which-key "edit init")
   "fek" '(sam--edit-keybindings :which-key "edit keybindings")
   "fef" '(sam--edit-functions :which-key "edit functions")
   "ff"  '(find-file :which-key "find file")
   "fo"  '(sam--open-in-external-app :which-key "open file")
   "fr"  '(counsel-recentf :which-key "recent files")
   "fs"  '(save-buffer :which-key "save file")
   "fS"  '(rename-file :which-key "rename file")

   ;; Jump to :
   "g" '(:ignore t :which-key "Go to")
   "gc" 'avy-goto-char
   "gC" 'avy-goto-char-2
   "gl" 'avy-goto-line
   "gé" 'avy-goto-word-or-subword-1

   ;; Insert
   "i" '(:ignore t :which-key "Insert")
   "it"  '(sam--insert-timestamp :which-key "timestamp")
   "il" '(:ignore t :which-key "insert link")
   "ilm" '(sam--chrome-md-link :which-key "chrome - md")
   "ilo" '(sam--chrome-org-link :which-key "chrome - org")
   "ilf" '(sam--finder-md-link :which-key "finder - md")

   ;; Journal
   "j" '(:ignore t :which-key "Journal")

   ;; Quit
   "q" '(:ignore t :which-key "Quit")
   "qb" 'kill-buffer-if-not-modified
   "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
   "qr" '(restart-emacs :which-key "restart emacs")

   ;; Save and search
   "s" '(:ignore t :which-key "Save/Search")
   "s."  'save-buffer

   ;; text related
   "t" '(:ignore t :which-key "text")
   "ta" '(:ignore t :which-key "text align")
   "tar" 'align-region
   "taR" 'align-regexp
   "ti"  'indent-region
   "tr" '(vr/query-replace :which-key "text replace")

   ;; Toggle UI elements
   "T" '(:ignore t :which-key "Toggle")
   "TF" '(toggle-frame-fullscreen :which-key "fullscreen")
   "Td" '(solarized-switch-to-dark :which-key "dark background")
   "Tl" '(solarized-switch-to-light :which-key "light background")
   "Tm" '(:ignore t :which-key "modeline")
   "Tmt" 'display-time-mode
   "Tn" '(linum-mode :which-key "line number")

   ;; Git related stuff
   "v" '(:ignore t :which-key "Version Control")
   "vb" 'magit-blame
   "vB" 'magit-blame-quit
   "vc" 'magit-commit
   "vC" 'magit-checkout
   "vd" 'magit-diff-unstaged
   "ve" 'magit-ediff-compare
   "vi" 'magit-init
   "vm" '(git-messenger:popup-message :which-key "git messenger")
   "vs" 'magit-status
   "vS" 'magit-stage-file
   "vt" 'git-timemachine
   "vU" 'magit-unstage-file
   "vv" 'magit-status
   )

  ;; this is the second prefix. It gives shorter access to common
  ;; functions. Like avy goto line.
  (general-define-key
   :states '(normal insert emacs)
   :prefix "C-SPC"
   :non-normal-prefix "C-SPC"
   "l" '(avy-goto-line)
   "a" 'align-regexp
   )

  ;; this is the last prefix. specifically reserved to emacs, like
  ;; getting help in emacs or stuff like that
  (general-define-key
   :states '(normal emacs)
   :prefix "ê"
   "" '(:ignore t :which-key "Emacs Help")
   "f" 'counsel-describe-function
   "k" 'counsel-descbinds
   "v" 'counsel-describe-variable
   "e" 'eval-last-sexp
   "b" 'eval-buffer
   "c" '(sam--eval-current-form-sp :which-key "eval-current")
   )

  ;; those are the direct keybindings. Just press the touch.
  (nmap
   "'" (general-simulate-keys "C-c")
   "é" 'evil-goto-mark
   "è" 'ace-window
   "s-b" 'ivy-switch-buffer
   "s-g" 'avy-goto-char
   "C-p" 'browse-kill-ring
   )

  (imap
   ;; restaure quelques commandes emacs par défault
   "C-a" 'beginning-of-line
   "C-d" 'delete-forward-char
   "C-e" 'end-of-line
   "C-f" 'forward-char
   "C-b" 'backward-char
   "C-n" 'evil-next-line
   "C-p" 'evil-previous-line
   )

  (mmap
   "t" 'evil-next-visual-line
   "s" 'evil-previous-visual-line
   )

  )


;;
;; C- map
;;



;;
;; SUPER map
;;

(bind-key* (kbd "s-l") #'sam--comment-or-uncomment-region-or-line)
(bind-key* (kbd "s-w") 'delete-other-windows)
(bind-key* [s-tab] 'sam--switch-to-other-buffer)


;;
;; HYPER map
;;

(bind-key  (kbd "H-F") 'toggle-frame-fullscreen)


;;
;; META map
;;

(bind-key  (kbd "M-«") 'beginning-of-buffer)
(bind-key  (kbd "M-»") 'end-of-buffer)


;;
;; Which-key
;;




;;
;; Hydra
;;

;; TODO: visual regexp. engin de remplacement
