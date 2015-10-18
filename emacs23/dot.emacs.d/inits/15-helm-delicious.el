(require 'helm-delicious)
(setq helm-c-delicious-cache-file "~/.emacs.d/.delicious.cache")

(require 'auth-source)
(if (file-exists-p "~/.authinfo.gpg")
    (setq auth-sources '((:source "~/.authinfo.gpg" :host t :protocol t)))
    (setq auth-sources '((:source "~/.authinfo" :host t :protocol t))))
