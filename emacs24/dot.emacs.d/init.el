;; (require 'cl) を見逃す
(setq byte-compile-warnings '(not cl-functions obsolete))

(require 'init-loader-x "~/.emacs.d/elisp/init-loader-x")
(init-loader-load "~/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(helm-ff-auto-update-initial-value nil)
 '(package-selected-packages
   '(ample-theme ample-zen-theme zenburn-theme which-key web-mode use-package rainbow-mode rainbow-delimiters powerline php-auto-yasnippets org-plus-contrib migemo markdown-mode js2-mode highlight-symbol helm-swoop helm-ls-git helm-gtags helm-descbinds helm-c-yasnippet flycheck-pos-tip expand-region auto-highlight-symbol auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
