;; c-mode
(add-to-list 'auto-mode-alist '("\\.c\\'"  . c-mode))
;; hook
(defun c-mode-hooks ()
  (helm-gtags-mode 1)
  (setq indent-tabs-mode               t)
  (setq tab-width                      4)
  (setq c-basic-offset                 4)
  (c-set-offset 'substatement-open'    0)

  ;; ruler
  (ruler-mode t)
)

(add-hook 'c-mode-hook 'c-mode-hooks)
