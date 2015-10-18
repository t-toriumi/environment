(use-package powerline
  :ensure powerline)

;; These two lines are just examples
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))

(powerline-default-theme)

(set-face-attribute 'mode-line nil
                    :foreground "#ffffff"
                    :background "#005faf"
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :foreground "#ffffff"
                    :background "#0087d7"
                    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
                    :foreground "#ffffff"
                    :background "#5c5cff"
                    :inherit 'mode-line)
