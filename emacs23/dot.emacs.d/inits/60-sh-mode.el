(defun sh-mode-settings ()
  (setq tab-width 2
        sh-basic-offset 2
        indent-tabs-mode nil)
  )

(add-hook 'sh-mode-hook 'sh-mode-settings)
