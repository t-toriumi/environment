;; js2-mode
(use-package js2-mode
  :ensure js2-mode)

;; auto load
(autoload 'js2-mode "js2-mode" "Major mode for editing js  code." t)
;; auto mode
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'"  . js2-mode))

;; hook
(defun js2-mode-hooks ()
  ;; style
  (setq indent-tabs-mode             nil)
  (setq tab-width                      2)
  (setq js2-basic-offset               2)
  (c-set-offset 'substatement-open'    0)
)

(add-hook 'js2-mode-hook 'js2-mode-hooks)
