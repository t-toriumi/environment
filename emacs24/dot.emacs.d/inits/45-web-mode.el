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
(add-to-list 'auto-mode-alist '("\\.twig\\'"  . web-mode))

;;; その他設定
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight  t)
(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))))

;; タグを自動で閉じる
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)

;; hook
(defun web-mode-hooks ()
  ;; インデント設定
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  ;; ruler
  (ruler-mode t)
  (linum-mode t)
)

;; 色
(custom-set-faces
 '(web-mode-block-face  ((t (:background nil))))
 '(web-mode-string-face ((t (:background nil))))
 '(web-mode-style-face  ((t (:background nil))))
 '(web-mode-part-face   ((t (:background nil))))
 '(web-mode-block-face  ((t (:background nil))))
 )

(add-hook 'web-mode-hook 'web-mode-hooks)
