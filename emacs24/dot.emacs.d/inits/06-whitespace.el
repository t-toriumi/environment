;; 日本語の空白表示
(require 'whitespace)

;; see whitespace.el for more details
(setq whitespace-style '(face trailing tabs empty tab-mark spaces space-mark))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")

(set-face-foreground 'whitespace-tab   "#949494")
(set-face-background 'whitespace-tab   nil)
(set-face-foreground 'whitespace-space "#ff0000")
(set-face-background 'whitespace-space nil)
(set-face-bold-p     'whitespace-space t)

;; 日本語空白の表示を全体に適用
(global-whitespace-mode 1)
