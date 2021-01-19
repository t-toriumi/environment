;; org-mode
;;(use-package org-plus-contrib
;;  :ensure t)
;;
;;(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;;
;;(use-package org-install)
;;(use-package org-capture)
;;
;;(setq org-startup-truncated nil)
;;(setq org-return-follows-link t)
;;(setq org-hide-leading-stars  t)
;;
;;;; default expand all
;;(setq org-startup-folded 'showall)
;;
;;(setq org-directory "~/.org/")
;;(setq org-agenda-files (list org-directory))
;;
;;;; todo status
;;(setq org-todo-keywords
;;      '((sequence "TODO(t)" "WAIT(w)" "DOING(d)" "|" "DONE(d)" "SOMEDAY(s)")))
;;
;;;; capture template
;;(setq org-capture-templates
;;      `(("t" "Todo" entry
;;         (file+headline "todo.org" "Todo")
;;         "** TODO %? :ALL:\n   %i\n   %t")
;;        ("m" "Memo" entry
;;         (file+headline "memo.org" "Memo")
;;         "** MEMO %? :ALL:\n   %i\n   %t")
;;        ("i" "Idea" entry
;;         (file+headline "idea.org" "Ideas")
;;         "** IDEA %? :ALL:\n   %i\n   %t")))
;;
;;;; custom agenda
;;(setq org-agenda-custom-commands
;;      '(("a" "Agenda and all TODO's"
;;         ((tags "ALL") (agenda "") (alltodo)))))
