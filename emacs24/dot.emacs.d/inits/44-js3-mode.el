;; js3-mode
(use-package js3-mode
  :ensure js3-mode)

;; auto load
(autoload 'js3-mode "js3-mode" "Major mode for editing js  code." t)
;; auto mode
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js3-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'"  . js3-mode))

;; hook
(defun js3-mode-hooks ()
  ;; style
  (setq indent-tabs-mode             nil)
  (setq tab-width                      2)
  (setq js3-basic-offset               2)
  (c-set-offset 'substatement-open'    0)
)

(add-hook 'js3-mode-hook 'js3-mode-hooks)
