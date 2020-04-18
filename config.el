;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq font-family "JetBrains Mono Medium")
;; (setq font-family "M+ 1m")
(setq font-family "Fantasque Sans Mono")

(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family font-family :size 20)
          doom-big-font (font-spec :family font-family :size 30))
  (setq doom-font (font-spec :family font-family :size 14)
        doom-big-font (font-spec :family font-family :size 24)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-snazzy)
;; For some readon, doom-theme remove font config

;; If you intend to use org, it is recommended you change this!
;; (setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;; Here lies my personnal configurations.
(setq fancy-splash-image "~/.doom.d/notarock.png")

;;; Imports
(load! "~/.doom.d/personal/my-org.el")
(load! "~/.doom.d/defuns/utils.el")

;;; Keybindings
;; Lets drag stuff aroung using hjk;
(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)

(map! :ne "SPC #" #'comment-or-uncomment-region)
(map! :ne "SPC =" #'indent-buffer)

(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)

(map! :ne "SPC s h" #'insert-random-hash)

;; On vsplit using V, focus the new frame
(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))

(map! [remap org-capture] nil)

(global-git-gutter-mode +1)

;; Set the padding between lines
;; (defvar line-padding 3)
;; (defun add-line-padding ()
  ;; "Add extra padding between lines"
                                        ;; ; remove padding overlays if they already exist
  ;; (let ((overlays (overlays-at (point-min))))
    ;; (while overlays
      ;; (let ((overlay (car overlays)))
        ;; (if (overlay-get overlay 'is-padding-overlay)
            ;; (delete-overlay overlay)))
      ;; (setq overlays (cdr overlays))))
                                        ;; ; add a new padding overlay
  ;; (let ((padding-overlay (make-overlay (point-min) (point-max))))
    ;; (overlay-put padding-overlay 'is-padding-overlay t)
    ;; (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
    ;; (overlay-put padding-overlay 'line-height (+ 1 (* .1 line-padding))))
  ;; (setq mark-active nil))

;; (add-line-padding)

;; Uncomment this to start in open maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

                                        ; Add line padding when font is ugly
;; (add-hook 'buffer-list-update-hook 'add-line-padding)

;; (use-package undo-fu-session)
