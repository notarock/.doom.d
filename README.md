Notarock\'s Doom-Emacs Literate Configuration
=============================================

This file is heavily inspired by doom-emacs\'s base \`config.el\` file.
In fact, it\'s just my personnal \`config.el\` translated to org-mode so

Basics
------

These are used for a number of things, particularly for GPG
configuration, some email clients, file templates and snippets.

``` {.commonlisp org-language="emacs-lisp"}
(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")
```

Theming
-------

Doom exposes five (optional) variables for controlling fonts in Doom.
Here are the three important ones:

-   \`doom-font\'
-   \`doom-variable-pitch-font\'
-   \`doom-big-font\' -- used for \`doom-big-font-mode\'

They all accept either a font-spec, font string (\"Input Mono-12\"), or
xlfd font string. You generally only need these two:

``` {.commonlisp org-language="emacs-lisp"}
;; (setq font-family "JetBrains Mono Medium")
;; (setq font-family "M+ 1m")
(setq font-family "Fantasque Sans Mono")

(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family font-family :size 20)
          doom-big-font (font-spec :family font-family :size 30))
  (setq doom-font (font-spec :family font-family :size 14)
        doom-big-font (font-spec :family font-family :size 24)))
```

There are two ways to load a theme. Both assume the theme is installed
and available. You can either set \`doom-theme\' or manually load a
theme with the \`load-theme\' function. These are the defaults.

``` {.commonlisp org-language="emacs-lisp"}
(setq doom-theme 'doom-snazzy)
```

want to change the style of line numbers, change this to \`relative\' or
\`nil\' to disable it:

``` {.commonlisp org-language="emacs-lisp"}
(setq display-line-numbers-type t)
```

Org-mode
--------

If you intend to use org, it is recommended you change this!

::: {.emacs-lisp}
;; (setq org-directory \"\~/org/\")
:::

Mics
----

Here are some additional functions/macros that could help you configure
Doom:

-   \`load!\' for loading external \*.el files relative to this one
-   \`use-package\' for configuring packages
-   \`after!\' for running code after a package has loaded
-   \`add-load-path!\' for adding directories to the \`load-path\',
    where Emacs looks when you load packages with \`require\' or
    \`use-package\'.
-   \`map!\' for binding new keys

To get information about any of these functions/macros, move the cursor
over the highlighted symbol at press \'K\' (non-evil users must press
\'C-c g k\'). This will open documentation for it, including demos of
how they are used. You can also try \'gd\' (or \'C-c g d\') to jump to
their definition and see how they are implemented.

Here lies my personnal configurations.
======================================

``` {.commonlisp org-language="emacs-lisp"}

(setq fancy-splash-image "~/.doom.d/notarock.png")

```

Additionnal files
-----------------

### Org-mode

``` {.commonlisp org-language="emacs-lisp"}
(load! "~/.doom.d/personal/my-org.el")
```

### Aditionnal functions

``` {.commonlisp org-language="emacs-lisp"}
(load! "~/.doom.d/defuns/utils.el")
```

Keybinds
--------

### Text manipulation

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)
```

### Comment, indents, etc

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC #" #'comment-or-uncomment-region)
(map! :ne "SPC =" #'indent-buffer)
```

### Project navigation

Uses dumb-jump to find functions, variables, and other definition

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)
```

### Text insertion

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC s h" #'insert-random-hash)
```

### Windows & frame manipulation

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))
```

Others
------

### Hacks

``` {.commonlisp org-language="emacs-lisp"}
(map! [remap org-capture] nil)
```

### Additionnal modes

``` {.commonlisp org-language="emacs-lisp"}
(global-git-gutter-mode +1)
```
