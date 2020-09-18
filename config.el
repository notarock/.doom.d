(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")

(setq doom-theme 'base16-helios)

(setq font-family "Hack")

(setq doom-font (font-spec :family font-family :size 18)
    doom-big-font (font-spec :family font-family :size 28))

(setq display-line-numbers-type t)

(setq fancy-splash-image "~/.doom.d/notarock.png")

(setq frame-resize-pixelwise t)

(defun my/set-initial-frame ()
  "Set initial frame size and position"
  (let* ((base-factor 0.80)
         (a-width (* (display-pixel-width) (/ base-factor 2)))
         (a-height (* (display-pixel-height) base-factor))
         (a-left (truncate (/ (- (display-pixel-width) a-width) 2)))
         (a-top (truncate (/ (- (display-pixel-height) a-height) 2))))
    (set-frame-position (selected-frame) a-left a-top)
    (set-frame-size (selected-frame) (truncate a-width)  (truncate a-height) t)))

(my/set-initial-frame)

(defun indent-buffer ()
  "Indent the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

(defun insert-random-hash ()
  (interactive)
  (insert (string-trim (shell-command-to-string "< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-64};echo;"))))

;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; (after! flycheck
  ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  ;; (flycheck-add-mode 'css-stylelint 'typescript-mode)
  ;; (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))
  ;; (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint))))

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(map! :map web-mode-map
      :n "SPC m F" #'eslint-fix)

(map! :map typescript-mode-map
      :n "SPC m F" #'eslint-fix)

(use-package org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

(setq org-directory "~/org/"
      org-todo-file (concat org-directory "todo.org")
      org-notes-file (concat org-directory "notes.org")
      org-journal-file (concat org-directory "journal.org"))

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

(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)

(map! :ne "SPC #" #'comment-or-uncomment-region)
(map! :ne "SPC =" #'indent-buffer)

(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)

(map! :ne "SPC i h" #'insert-random-hash)

(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))

(map! [remap org-capture] nil)

;;  (shell-command "pandoc config.org -o README.md")

(global-git-gutter-mode +1)
