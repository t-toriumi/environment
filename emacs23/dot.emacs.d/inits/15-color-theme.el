;; Workaround to use color-theme-calm-forest provided by emacs-goodies-el
;; emacs23 way
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

;; emacs24 way
;; (package-initialize)
;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize)
;; (color-theme-calm-forest)
