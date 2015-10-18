;; build-in

;; 行番号の書式
(setq linum-format "%5d ")
(add-hook 'linum-mode-hook
          '(lambda()
             (set-face-foreground 'linum "#ffffff")
             (set-face-background 'linum "#444444")))

;; 行番号を表示するmode
(dolist (hook (list
           'emacs-lisp-mode-hook
           'c-mode-hook
           'php-mode-hook
           'css-mode-hook
           'js2-mode-hook))
  (add-hook hook (lambda () (linum-mode t))))

