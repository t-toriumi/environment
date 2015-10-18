;;(require 'html-mode)

(defun html-mode-settings ()
  (setq tab-width 2
        c-basic-offset 2
        indent-tabs-mode nil)
)
(defun cakephp/html-mode-settings ()
  (setq tab-width 2
        c-basic-offset 2
        indent-tabs-mode t)
)

(add-hook 'html-mode-hook 'cakephp/html-mode-settings)
