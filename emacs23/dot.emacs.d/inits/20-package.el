(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/"))
      package-enable-at-startup nil)
(package-initialize)

(defvar installing-package-list
  '(
    ac-c-headers
    ac-dabbrev
    ac-etags
    ac-helm
;;    ac-js2
    auto-compile
    auto-complete
    ctags
;;    color-theme
    elscreen
    eydropper
    facemenu+
    flymake
    flymake-cursor
    flymake-easy
    flymake-jshint
    flymake-json
    flymake-php
    flymake-puppet
    flymake-ruby
    flymake-shell
    flymake-yaml
    fuzzy
    geben
    helm
    helm-delicious
    helm-dired-recent-dirs
    helm-flymake
    helm-gtags
    helm-rails
    helm-rb
    helm-rubygems-local
;;    hl-line
    inf-php
    jshint
    magit
    mew
    org
    psvn
    pos-tip
    vagrant
    w3m
    yasnippet

    android-mode
    google-c-style
    js3-mode
    markdown-mode
;;    php-auto-yasnippet
    php-extras
    php-mode
    rinari
    rspec-mode
    ruby-block
    ruby-mode
    ruby-electric
    ruby-end
    yaml-mode
    puppet-mode
    multi-web-mode

    google-maps
    google-translate
    tumble
    twitterling-mode
    ))
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg installing-package-list)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))
