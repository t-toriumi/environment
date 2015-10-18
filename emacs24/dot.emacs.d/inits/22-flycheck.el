(use-package flycheck
  :ensure flycheck)

(use-package flycheck-pos-tip
  :ensure flycheck-pos-tip)

;; flycheck pos-tip
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
