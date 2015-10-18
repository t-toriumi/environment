(require 'php-mode)
(require 'php-completion)
;;(require 'php-electric)
(require 'multi-web-mode)

(setq auto-mode-alist
      (append
       '(
         ("\\.inc$" . php-mode)
         ("\\.php$" . php-mode)
         ("\\.ctp$" . php-mode) ;; cakephp template
         ) auto-mode-alist))
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

(defun php-mode-settings ()
  (php-enable-symfony2-coding-style)
  (setq php-search-url "http://www.php.net/"
        php-manual-url "http://www.php.net/manual/"
        php-manual-path "/usr/share/doc/php-doc/html"
        tab-width 2
        c-basic-offset 2
        indent-tabs-mode nil)
  (c-set-offset 'substatement-open' 0)

  (defun php-search-documentation ()
  "Search PHP documentation for the word at the point."
  (interactive)
  (w3m-browse-url (concat php-search-url (current-word t))))

  (defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (w3m-browse-url php-manual-url))

  ;; Enable flyspell for comments in source code
  (flyspell-prog-mode)

  (php-completion-mode t)
  (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
)
(defun cakephp/php-mode-settings ()
  (php-enable-symfony2-coding-style)
  (setq php-search-url "http://www.php.net/"
        php-manual-url "http://www.php.net/manual/"
        php-manual-path "/usr/share/doc/php-doc/html"
        tab-width 2
        c-basic-offset 2
        indent-tabs-mode t)
  (c-set-offset 'substatement-open' 0)

  (defun php-search-documentation ()
  "Search PHP documentation for the word at the point."
  (interactive)
  (w3m-browse-url (concat php-search-url (current-word t))))

  (defun php-browse-manual ()
  "Bring up manual for PHP."
  (interactive)
  (w3m-browse-url php-manual-url))

  ;; Enable flyspell for comments in source code
  (flyspell-prog-mode)

  (php-completion-mode t)
  (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
)

(make-variable-buffer-local 'ac-sources)
(add-hook 'php-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t)
               (add-to-list 'ac-sources 'ac-source-php-completion)
               (auto-complete-mode t)
               )))

;; Disabled to workaround php-electric-mode hook conflicts with yasnippet
;;(add-hook 'php-mode-hook 'php-electric-mode)
;;(add-hook 'php-mode-hook 'php-mode-settings)
(add-hook 'php-mode-hook 'cakephp/php-mode-settings)
(add-hook 'php-mode-hook 'helm-gtags-mode)
(add-hook 'php-mode-hook 'flymake-php-load)
;;(add-hook 'php-mode-hook 'turn-on-ctags-auto-update-mode)

;; Workaround to disable flymake while using magit-ediff
;; See http://qiita.com/akisute3@github/items/54f0387284ac57c106c0
(defadvice magit-ediff (around flymake-off activate)
  (remove-hook 'php-mode-hook 'flymake-php-load)
  ad-do-it
  (add-hook 'php-mode-hook 'flymake-php-load))
