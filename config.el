(setq user-full-name "Roch D'Amour"
      user-mail-address "roch.damour@gmail.com")

(setq doom-theme 'doom-monokai-classic)

(setq font-family "Essential PragmataPro")

(setq doom-font (font-spec :family font-family :size 22)
    doom-big-font (font-spec :family font-family :size 34))

(setq display-line-numbers-type t)

(setq fancy-splash-image "~/.doom.d/notarock.png")

(setq frame-resize-pixelwise t)

; (feebleline-mode +1)
(dimmer-configure-magit)
(dimmer-configure-org)
(dimmer-mode t)
(setq highlight-indent-guides-method 'fill)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

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

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(setq lsp-python-ms-executable (executable-find "python-language-server"))

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

(after! org
  (map! :map org-mode-map
        :n "m-j" #'org-metadown
        :n "m-k" #'org-metaup)
  (setq org-bullets-bullet-list '("▶")
        org-directory "~/org/"
        org-journal-file (concat org-directory "journal.org")
        org-todos-file (concat org-directory "todos.org")
        org-capture-templates       (doct '(("Journal" :keys "j"
                                             :file org-journal-file
                                             :datetree t
                                             :todo nil
                                             :template ("* %^{description}"
                                                        ":properties:"
                                                        ":created: %u"
                                                        ":end:"
                                                        "%?"))
                                            ("Todos" :keys "t"
                                             :file org-todos-file
                                             :todo-state "TODO"
                                             :template ("* TODO: %^{description}"
                                                        ":properties:"
                                                        ":created: %u"
                                                        ":end:"))
                                            ))
        org-todo-keyword-faces (quote (("todo" :foreground "#ff6347" :weight bold)
                                       ("done" :foreground "#006400" :weight bold :strike-through t)))
        org-todo-keywords '((sequence "todo(t)" "done(d)"))
        org-log-done t))

(map! :ne "C-S-k" #'drag-stuff-up)
(map! :ne "C-S-j" #'drag-stuff-down)
(map! :ne "C-S-l" #'drag-stuff-right)
(map! :ne "C-S-h" #'drag-stuff-left)

(map! :ne "SPC =" #'indent-buffer)
(map! :ne "SPC #" #'comment-or-uncomment-region)

(map! :ne "SPC j g" #'dumb-jump-go)
(map! :ne "SPC j b" #'dumb-jump-back)

(map! :ne "SPC i h" #'insert-random-hash)

(map! :ne "SPC w V" (lambda () (interactive)(evil-window-vsplit) (other-window 1)))

(map! [remap org-capture] nil)

;;  (shell-command "pandoc config.org -o README.md")

(global-git-gutter-mode +1)

(global-wakatime-mode +1)
