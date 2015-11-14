(use-package yasnippet
  :ensure yasnippet)


(setq yas-prompt-functions '(yas-no-prompt))
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        ))

;; helmで利用するのでTABは利用しない
(define-key yas-minor-mode-map (kbd "TAB") nil)
(yas-global-mode 1)
