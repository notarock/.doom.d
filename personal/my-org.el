;;; ~/.doom.d/personal/my-org.el -*- lexical-binding: t; -*-
;; Contains All org-mode related configuration

(def-package! org-fancy-priorities
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
        org-capture-templates '(("j" "Journal" entry (file+oldp+datetree org-journal-file)
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
