;;; ~/.doom.d/my-org.el -*- lexical-binding: t; -*-
;; Contains All org-mode related configuration

(def-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("■" "■" "■")))

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

(setq org-directory "~/org/")
