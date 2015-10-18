(require 'js3-mode)
(require 'flymake-jshint)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(define-key js3-mode-map (kbd "M-n") 'js3-next-error)
(define-key js3-mode-map (kbd "M-p") 'js3-prev-error)

;; (custom-set-variables
;;  '(js3-auto-indent-p t)
;;  '(js3-enter-indents-newline t)
;;  '(js3-indent-on-enter-key nil))

(defun js-mode-settings ()
  (setq js3-indent-level 2
        js3-expr-indent-offset 2
        indent-tabs-mode nil)
  )

(add-hook 'js3-mode-hook 'js-mode-settings)
(add-hook 'js3-mode-hook 'flymake-jshint-load)

(defun unload-generic-x ()
  (unload-feature 'generic-x))
