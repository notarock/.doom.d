;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

;; themes are THE MOST important setting
(require 'base16-theme)

;; Fonts too
(setq doom-theme 'base16-helios
      display-line-numbers-type 'relative)

(global-git-gutter-mode +1)

(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family "Iosevka" :size 20)
          doom-big-font (font-spec :family "Iosevka" :size 36))
  (setq doom-font (font-spec :family "Iosevka" :size 16)
        doom-big-font (font-spec :family "Iosevka" :size 30)))

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

;; Load additionnal configuration packages
(load-file "~/.doom.d/personal/my-org.el")
(load-file "~/.doom.d/defuns/utils.el")

(map! [remap org-capture] nil)


(setq fancy-splash-image "~/.doom.d/notarock.png")
