;; load theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/elisp/themes/")
;; (load-theme 'kamome t)
;; (enable-theme 'kamome)

(setq zenburn-override-colors-alist
      '(
        ("zenburn-bg"       . "#101010")
        ("zenburn-bg-1"     . "#383838")
        ))

(load-theme 'zenburn t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-dotted-directory ((t (:extend t :background "#101010" :foreground "black"))))
 '(helm-ff-dotted-symlink-directory ((t (:extend t :background "#101010" :foreground "DarkOrange"))))
)
