;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

;; themes are THE MOST important setting
(require 'base16-theme)

;; Fonts too
(setq doom-theme 'base16-hopscotch
      fancy-splash-image "~/.doom.d/notarock.png"
      display-line-numbers-type 'relative)


(global-git-gutter-mode +1)

(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family "Monoid" :size 16)
          doom-big-font (font-spec :family "Monoid" :size 30))
  (setq doom-font (font-spec :family "Monoid" :size 12)
        doom-big-font (font-spec :family "Monoid" :size 24)))



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

;; Set the padding between lines
(defvar line-padding 2)
(defun add-line-padding ()
  "Add extra padding between lines"

  ; remove padding overlays if they already exist
  (let ((overlays (overlays-at (point-min))))
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'is-padding-overlay)
            (delete-overlay overlay)))
      (setq overlays (cdr overlays))))

  ; add a new padding overlay
  (let ((padding-overlay (make-overlay (point-min) (point-max))))
    (overlay-put padding-overlay 'is-padding-overlay t)
    (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
    (overlay-put padding-overlay 'line-height (+ 1 (* .1 line-padding))))
  (setq mark-active nil))


;; Uncomment this to start in open maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'buffer-list-update-hook 'add-line-padding)
