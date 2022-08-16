(use-package helm-ag
  :ensure helm-ag)

(custom-set-variables
 '(helm-ag-base-command "ag -u --nocolor --nogroup --ignore-case")
 '(helm-ag-insert-at-point 'symbol)
 )

