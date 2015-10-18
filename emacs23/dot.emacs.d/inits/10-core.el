;; (menu-bar-mode 0)
;; (setq inhibit-startup-message t)
;; (setq gc-cons-threshold 8000000)

;; ;; set charset
;; (set-language-environment "Japanese")
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (setq buffer-file-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (setq split-width-threshold nil)

;; (setq user-mail-address "topaz2.3333@gmail.com")

;; ;; set lib path
;; (let ((default-directory (expand-file-name "~/.emacs.d")))
;;   (add-to-list 'load-path default-directory)
;;   (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
;;       (normal-top-level-add-subdirs-to-load-path)))

;; (require 'init-loader)
;; (setq init-loader-show-log-after-init nil)
;; (init-loader-load "~/.emacs.d/inits")

;; ;; store all back file in ~/.ebackup
;; (setq backup-directory-alist '(("\\.*" . "~/.ebak")))

;; ;; convert yes/no to y/n
;; (fset 'yes-or-no-p 'y-or-n-p)

;; ;; custom key bindings
;; (global-set-key "\C-h" 'backward-delete-char)
;; ;; (global-set-key "\C-w" 'backward-kill-word)
;; ;; (global-set-key "\M-w" 'kill-region)
;; (global-set-key "\M-?" 'help)
;; (global-set-key "\C-cc" 'comment-region)
;; (global-set-key "\C-cu" 'uncomment-region)
;; (global-set-key "\M-g" 'goto-line)

;; ;; byte compile
;; (if (file-newer-than-file-p "~/.emacs" "~/.emacs.elc" )
;;     (save-excursion
;;       (byte-compile-file "~/.emacs")))

;; (add-hook 'after-save-hook
;;     (function (lambda ()
;;           (if (eq major-mode 'emacs-lisp-mode)
;;         (save-excursion
;;           (byte-compile-file buffer-file-name))))))
