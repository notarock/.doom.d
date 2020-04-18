(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")

;; (setq font-family "JetBrains Mono Medium")
;; (setq font-family "M+ 1m")
(setq font-family "Fantasque Sans Mono")

(if (equal (display-pixel-width) 2560)
    (setq doom-font (font-spec :family font-family :size 20)
          doom-big-font (font-spec :family font-family :size 30))
  (setq doom-font (font-spec :family font-family :size 14)
        doom-big-font (font-spec :family font-family :size 24)))

(setq doom-theme 'doom-snazzy)

(setq display-line-numbers-type t)

(setq fancy-splash-image "~/.doom.d/notarock.png")

(load! "~/.doom.d/personal/my-org.el")

(load! "~/.doom.d/defuns/utils.el")

(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)

(map! :ne "SPC #" #'comment-or-uncomment-region)
(map! :ne "SPC =" #'indent-buffer)

(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)

(map! :ne "SPC s h" #'insert-random-hash)

(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))

(map! [remap org-capture] nil)

(global-git-gutter-mode +1)
