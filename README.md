# Emacs repository

Ceci est mon repository emacs.

To install simply links `~/.emacs.d/init.el` to
`~/wherever/you/put/this/repository/init.el`.

Then edit `~/.emacs.d/init.el` and change the `(load-file )` declarations at the
end of the file to point to the `org.el`, `functions.el` and `keybindings.el`.

- functions.el: my own functions, taken from here and there.
- init.el: the init file. use-package declaration and all.
- keybindings.el: I let you guess this one.
- org.el: org-mode setup.
