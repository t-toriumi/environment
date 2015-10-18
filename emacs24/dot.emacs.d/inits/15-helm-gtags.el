(use-package helm-gtags
  :ensure helm-gtags)

(autoload 'helm-gtags-mode "helm-gtags" "" t)

;; hook
(defun helm-gtags-mode-hooks ()
)

(add-hook 'helm-gtags-mode-hook 'helm-gtags-mode-hooks)
