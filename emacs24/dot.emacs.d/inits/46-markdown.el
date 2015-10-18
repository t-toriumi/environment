;; markdown
(use-package markdown-mode
  :ensure markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; markdown command
(setq markdown-command "perl /usr/local/bin/Markdown.pl")
