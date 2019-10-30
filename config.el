;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Set initial frame size and position
(defun my/set-initial-frame ()
  (let* ((base-factor 0.85)
         (a-width (* (display-pixel-width) base-factor))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))
(setq frame-resize-pixelwise t)
(my/set-initial-frame)

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; Additionnal org configuration
(def-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

(map! :ne "SPC #" #'comment-or-uncomment-region)
;;
;; Lets drag stuff aroung using hjk;
(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-h" #'drag-stuff-left)
(map! :ne "C-S-l" #'drag-stuff-right)

(map! :ne "SPC #" #'comment-or-uncomment-region)
(map! :ne "SPC =" #'indent-buffer)

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-bullets-bullet-list '("◆")
        org-todo-keywords '((sequence
                             "TODO(t)"
                             "INPROGRESS(p)"
                             "BLOCKED(b)"
                             "WAITING(b)"
                             "|"
                             "CANCELLED(c!)"
                             "DONE(F!)"))))

(setq doom-font (font-spec :family "Pragmata Pro Mono" :size 20)
      doom-big-font (font-spec :family "Pragmata Pro Mono" :size 36)
      org-directory "~/org/")

;; Pour JS mode
;; js2-basic-offset 2
;; js-indent-level 2)

;; Place your private configuration here
