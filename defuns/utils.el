;;; ~/.doom.d/defuns/utils.el -*- lexical-binding: t; -*-
;; Contain additionnal functions

;; Function to set position/size on startup
(defun my/set-initial-frame ()
  "Set initial frame size and position"
  (let* ((base-factor 0.90)
         (a-width (* (display-pixel-width) base-factor))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

;; Set initial position
(setq frame-resize-pixelwise t)
(my/set-initial-frame)

(defun indent-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
