;; web-mode
(use-package web-mode
  :ensure web-mode)

;; emacs 23以下の互換
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))

;; auto load
(autoload 'web-mode "web-mode" "Major mode for editing web code." t)

;; auto mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'"   . web-mode))

;;; その他設定
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight  t)
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))))
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)

;; hook
(defun web-mode-hooks ()
  ;; style
  (setq web-mode-html-offset          2)
  (setq web-mode-css-offset           2)
  (setq web-mode-script-offset        2)
  (setq web-mode-php-offset           4)
  (setq web-mode-java-offset          4)
  (setq web-mode-asp-offset           4)
  (setq web-mode-markup-indent-offset 2)
  ;; ruler
  (ruler-mode t)
  (linum-mode t)
)
(add-hook 'web-mode-hook 'web-mode-hooks)
