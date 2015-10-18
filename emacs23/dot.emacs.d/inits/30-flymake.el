(require 'flymake nil t)
(require 'flymake-cursor)

(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "yellow")

(global-set-key (kbd "C-c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-c p") 'flymake-goto-prev-error)
