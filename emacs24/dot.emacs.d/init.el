;; (require 'cl) を見逃す
(setq byte-compile-warnings '(not cl-functions obsolete))

(require 'init-loader-x "~/.emacs.d/elisp/init-loader-x")
(init-loader-load "~/.emacs.d/inits")
