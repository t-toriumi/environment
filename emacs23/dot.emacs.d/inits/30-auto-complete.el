(require 'popup)
(require 'fuzzy)
(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)
(ac-flyspell-workaround)

(global-auto-complete-mode t)
(setq ac-auto-show-menu t
      ac-dwim t
      ac-use-menu-map t
      ac-quick-help-delay 1
      ac-quick-help-height 1
      ac-disable-inline t
      ac-show-menu-immediately-on-auto-complete t
      ac-auto-start 1
      ac-candidate-menu-min 0)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode php-mode ruby-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode))
  (add-to-list 'ac-modes mode))
