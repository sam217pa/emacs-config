# Emacs repository

This is my emacs repo, based on `use-package`.

To install simply link `~/.emacs.d/init.el` to
`~/wherever/you/put/this/repository/init.el`.

Then edit `~/.emacs.d/init.el` and change the `(load-file )`
declarations to point to the correct `org.el`, `functions.el` and
`keybindings.el`, or place this file into `~/dotfile/emacs/`.

- functions.el: my own functions, taken from here and there.
- init.el: the init file. use-package declaration and all.
- keybindings.el: I let you guess this one.
- org.el: org-mode setup.
- ess-config.el: configuration related to R and Julia code.
- python-config.el: setup anaconda-mode and all.
- latex-config.el
- elfeed.org: org-mode tree structure of pages I like to check.
