(require 'ruby-mode)
(require 'rinari)
(global-rinari-mode)

(mapc
 (lambda (file)
   (add-to-list 'auto-mode-alist
                (cons (concat (regexp-quote file) "\\'") 'ruby-mode)))
 '(".erb" "Berksfile" "Capfile" "Gemfile" "Guardfile" "Rakefile"
   "Vagrantfile"))

(defun ruby-mode-settings ()
  (setq tab-width 2
        ruby-indent-level tab-width
        ruby-deep-indent-paren-style nil)
  (define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)

  ;; Enable flyspell for comments in source code
  (flyspell-prog-mode)

  (require 'ruby-block)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t)

  (require 'ruby-electric)
  (require 'ruby-end)
)

(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'ruby-mode-settings)
(add-hook 'ruby-mode-hook 'helm-gtags-mode)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(require 'rcodetools)
(define-key ruby-mode-map (kbd "C-c C-d") 'xmp)

;; rsense
(setq rsense-home "/usr/local/src/rsense/0.3/libexec")
(require 'rsense)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-to-list 'ac-sources 'ac-source-rsense-method)
             (add-to-list 'ac-sources 'ac-source-rsense-constant)
             (define-key ruby-mode-map (kbd "C-x .") 'ac-complete-rsense)))
