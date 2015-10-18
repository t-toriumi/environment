(deftheme kamome
  "kamome theme")

(custom-theme-set-faces
 'kamome
 ;; 背景・文字・カーソル
 '(cursor  ((t (:foreground "#cd0000"))))
 '(default ((t (:background "#d0d0d0" :foreground "#000000"))))
 '(bold ((t (:bold nil :weight normal))))
 '(bold-italic ((t (:italic nil :bold nil :slant normal :weight normal))))
 ;; 選択範囲
 '(region ((t (:background "#cdcd00"))))
 ;; モードライン
 '(mode-line           ((t (:foreground "#F8F8F2" :background "#000000" :box (:line-width 1 :color "#000000" :style released-button)))))
 '(mode-line-inactive  ((t (:foreground "#BCBCBC" :background "#767676" :box (:line-width 1 :color "#333333")))))
 '(mode-line-buffer-id ((t (:foreground nil :background nil))))
 ;; ハイライト
 '(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
 '(hl-line ((t (:background "#293739"))))
 ;; font lock
 '(font-lock-function-name-face ((t (:foreground "#ff0000"))))
 '(font-lock-type-face          ((t (:foreground "#cd0000"))))
 '(font-lock-variable-name-face ((t (:foreground "#00cdcd"))))
 '(font-lock-string-face        ((t (:foreground "#af0087"))))
 '(font-lock-keyword-face       ((t (:foreground "#0000d7"))))
 '(font-lock-builtin-face       ((t (:foreground "#0000d7"))))
 '(font-lock-constant-face      ((t (:foreground "#0000d7"))))
 '(font-lock-error-face         ((t (:foreground "#cd0000"))))
 '(font-lock-warning-face       ((t (:foreground "#cd0000"))))
 '(font-lock-comment-face       ((t (:foreground "#008700"))))
 ;; Paren
 '(show-paren-match-face ((t (:foreground "#1B1D1E" :background "#949494"))))
 ;; Popup
 '(popup-face ((t (:foreground "#000000" :background "#ffffff"))))
 ;; CSS
 '(css-selector ((t (:foreground "#0000ee"))))
 '(css-property ((t (:foreground "#ff0000"))))
 ;; WEB
 '(web-mode-html-tag-bracket-face ((t (:foreground "#0000ee"))))
 '(web-mode-html-tag-face ((t (:foreground "#0000ee"))))
 '(web-mode-doctype-face ((t (:foreground "#cd0000"))))
 '(web-mode-html-attr-name-face ((t (:foreground "#cd0000"))))
 '(web-mode-current-element-highlight-face ((t (: "#cd0000"))))
 '(web-mode-param-name-face ((t (:foreground "#000000"))))
 )
 ;;; ###autoload
 (when load-file-name
   (add-to-list 'custom-theme-load-path
                               (file-name-as-directory (file-name-directory load-file-name))))
(provide-theme 'kamome)
