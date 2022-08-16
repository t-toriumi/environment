;; (require 'cl) を見逃す
(setq byte-compile-warnings '(not cl-functions obsolete))

(require 'init-loader-x "~/.emacs.d/elisp/init-loader-x")
(init-loader-load "~/.emacs.d/inits")
(put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c4cecd97a6b30d129971302fd8298c2ff56189db0a94570e7238bc95f9389cfb" default))
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(helm-ag-base-command "ag -u --nocolor --nogroup --ignore-case")
 '(helm-ag-insert-at-point 'symbol)
 '(helm-ff-auto-update-initial-value nil)
 '(package-selected-packages
   '(zenburn-theme markdown-mode web-mode js2-mode php-auto-yasnippets php-mode which-key migemo flycheck-pos-tip flycheck auto-highlight-symbol highlight-symbol expand-region helm-swoop helm-ls-git helm-gtags helm-descbinds helm-c-yasnippet helm-ag auto-complete rainbow-delimiters rainbow-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-dotted-directory ((t (:extend t :background "#101010" :foreground "black"))))
 '(helm-ff-dotted-symlink-directory ((t (:extend t :background "#101010" :foreground "DarkOrange"))))
 '(web-mode-block-face ((t (:background nil))))
 '(web-mode-part-face ((t (:background nil))))
 '(web-mode-string-face ((t (:background nil))))
 '(web-mode-style-face ((t (:background nil)))))
