Notarock\'s Doom-Emacs Literate Configuration
=============================================

This file is heavily inspired by doom-emacs\'s base \`config.el\` file.
In fact, it\'s just my personnal \`config.el\` translated to org-mode so

User config
-----------

These are used for a number of things, particularly for GPG
configuration, some email clients, file templates and snippets.

``` {.commonlisp org-language="emacs-lisp"}
(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")
```

Theming
-------

THis function is probably the most important peice of configuration in
this whole repository.

``` {.commonlisp org-language="emacs-lisp"}
(setq doom-theme 'doom-snazzy)
```

### Font

Doom exposes five (optional) variables for controlling fonts in Doom.
Here are the three important ones:

-   \`doom-font\'
-   \`doom-variable-pitch-font\'
-   \`doom-big-font\' -- used for \`doom-big-font-mode\'

They all accept either a font-spec, font string (\"Input Mono-12\"), or
xlfd font string.

``` {.commonlisp org-language="emacs-lisp"}
(setq font-family "Fantasque Sans Mono")
```

This snipper adjust the font size based on if I am currently using emacs
on my HiDPI laptop screen, or on anything else.

TODO: Maybe add more options?

``` {.commonlisp org-language="emacs-lisp"}
(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family font-family :size 20)
          doom-big-font (font-spec :family font-family :size 30))
  (setq doom-font (font-spec :family font-family :size 14)
        doom-big-font (font-spec :family font-family :size 24)))
```

want to change the style of line numbers, change this to \`relative\' or
\`nil\' to disable it:

``` {.commonlisp org-language="emacs-lisp"}
(setq display-line-numbers-type t)
```

### Splash screen picture

Set this picture as the splash image

![Notarock\'s splash image](./notarock.png "notarock.png")

``` {.commonlisp org-language="emacs-lisp"}
(setq fancy-splash-image "~/.doom.d/notarock.png")
```

### Enable pixelwise resizing

``` {.commonlisp org-language="emacs-lisp"}
(setq frame-resize-pixelwise t)
```

Aditionnal functions
--------------------

This function reposition Emacs on startup:

-   Width and height takes up 80% of the screen.
-   Emacs is positionned right in the certer of the.

``` {.commonlisp org-language="emacs-lisp"}
(defun my/set-initial-frame ()
  "Set initial frame size and position"
  (let* ((base-factor 0.80)
         (a-width (* (display-pixel-width) base-factor))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
```

Launch the function.

``` {.commonlisp org-language="emacs-lisp"}
(my/set-initial-frame)
```

Top to bottom file indentation.

``` {.commonlisp org-language="emacs-lisp"}
(defun indent-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
```

Insert a generated 64 character long random string.

``` {.commonlisp org-language="emacs-lisp"}
(defun insert-random-hash ()
  (interactive)
  (insert (string-trim (shell-command-to-string "< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-64};echo;"))))
```

Langage-specific
----------------

### Typescript

``` {.commonlisp org-language="emacs-lisp"}

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(after! flycheck
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'css-stylelint 'typescript-mode)
  (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))
  (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint))))
```

Mode-specific
-------------

### Org-mode

Contains All org-mode related configuration

Good looking bullet point, all about the eye-candy

``` {.commonlisp org-language="emacs-lisp"}
(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))
```

Define org files path

``` {.commonlisp org-language="emacs-lisp"}
(setq org-directory "~/org/"
      org-todo-file (concat org-directory "todo.org")
      org-notes-file (concat org-directory "notes.org")
      org-journal-file (concat org-directory "journal.org"))
```

Stuff that get loaded in when org-mode is initiated

``` {.commonlisp org-language="emacs-lisp"}
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-bullets-bullet-list '("◆")
        org-capture-templates '(("j" "Journal" entry (file+datetree org-journal-file)
                                 "* %?\nEntered on %U\n")
                                ("t" "Todo:" entry (file+headline org-todo-file "Todo List")
                                 "* TODO: %?\nEntered on %U\n")
                                ("n" "Note" entry (file org-notes-file)
                                 "* NOTE %?\n%U" :empty-lines 1)
                                ("N" "Note with Clipboard" entry (file org-notes-file)
                                 "* NOTE %?\n%U\n   %c" :empty-lines 1))
        org-todo-keyword-faces (quote (("TODO" :foreground "firebrick2" :weight bold)
                                       ("DONE" :foreground "OliveDrab2" :weight bold :strike-through t)
                                       ("CANCELLED" :foreground "chocolate1" :weight bold :strike-through t)
                                       ("WAITING" :foreground "cyan4" :weight bold)))
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w)" "|" "CANCELLED(c)"))
        org-log-done t))
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

Binds random hash to \"space \[i\]nsert \[h\]ash\"

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC i h" #'insert-random-hash)
```

### Windows & frame manipulation

Split the current window and focus the newly created frame

``` {.commonlisp org-language="emacs-lisp"}
(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))
```

Others
------

### Hacks

Org capture weird behaviour fix

``` {.commonlisp org-language="emacs-lisp"}
(map! [remap org-capture] nil)
```

Function used to recompile this repository\'s README

``` {.commonlisp org-language="emacs-lisp"}
;;  (shell-command "pandoc config.org -o README.md")
```

### modes

Enable global git-gutter-mode

``` {.commonlisp org-language="emacs-lisp"}
(global-git-gutter-mode +1)
```

### Hooks

sds
