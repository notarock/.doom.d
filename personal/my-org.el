;;; ~/.doom.d/personal/my-org.el -*- lexical-binding: t; -*-
;; Contains All org-mode related configuration

(def-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

(setq org-directory "~/org/"
      org-journal-file (concat org-directory "journal.org")
      org-journal-file (concat org-directory "journal.org")
      org-journal-file (concat org-directory "journal.org"))

(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  (setq org-bullets-bullet-list '("◆")
        org-todo-keyword-faces (quote (("TODO" :foreground "#dc322f" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold :strike-through t)
                                       ("WAITING" :foreground "turquoise" :weight bold)))
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w)" "|" "CANCELLED(c)"))
        org-log-done t))
