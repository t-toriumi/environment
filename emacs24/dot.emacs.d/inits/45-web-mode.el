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

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-comment-keywords t)

(setq web-mode-auto-close-style 2)
(setq web-mode-tag-auto-close-style 2)


;; hook
(defun web-mode-hooks ()
  ;; インデント設定
  ;; htmlのインデント
  (setq web-mode-markup-indent-offset 4)
  ;; CSSのインデント
  (setq web-mode-css-indent-offset 2)
  ;; Scriptのインデント
  (setq web-mode-code-indent-offset 2)
  ;; <style>の行頭
  (setq web-mode-style-padding 0)
  ;; <script>の行頭
  (setq web-mode-script-padding 0)

  ;; ruler
  (ruler-mode t)
  (linum-mode t)

  (defvar ac-source-css-property-names
    '((candidates . (loop for property in ac-css-property-alist
                          collect (car property)))))

  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-css-property-names
          ac-source-css-property
          ac-source-words-in-same-mode-buffers
          ac-source-dictionary))

  ;; 手動で補完せずTABで補完開始(トリガーキー)
  (setq ac-auto-start nil)
  (setq ac-dwim nil)
  (ac-set-trigger-key "TAB")
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
