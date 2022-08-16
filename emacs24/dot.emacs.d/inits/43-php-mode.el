;; php-mode
(use-package php-mode
  :ensure php-mode)

;;(use-package php-auto-yasnippets
;;    :ensure t)

;; auto load
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
;; auto mode
(add-to-list 'auto-mode-alist '("\\.php\\'"  . php-mode))

;; hook
(defun php-mode-hooks ()
  ;; minor-mode
  (helm-gtags-mode 1)
  (yas-minor-mode 1)
  ;;(flycheck-mode 1)

  ;; style
  (setq indent-tabs-mode               nil)
  (setq tab-width                      4)
  (setq c-basic-offset                 4)
  (c-set-offset 'substatement-open'    0)

  ;; ;; php-completion
  ;; (require 'php-completion)
  ;; ;;(use-package php-completion
  ;; ;;  :ensure php-completion)

  ;; (php-completion-mode t)

  ;; php-completoin & auto-complete
  ;; (make-local-variable 'ac-sources)
  ;; (setq ac-sources '(
  ;;                    ac-source-words-in-same-mode-buffers
  ;;                    ac-source-php-completion
  ;;                    ac-source-filename
  ;;                   ac-source-yasnippet))
)

(add-hook 'php-mode-hook 'php-mode-hooks)
(add-hook 'php-mode-hook 'smartchr-keybindings-php)
