(deftheme kamome
  "kamome theme")

(custom-theme-set-faces
 'kamome
 ;; 背景・文字・カーソル
 ;;'(cursor  ((t (:foreground "#cd0000"))))
 '(default ((t (:foreground "#ffffff" :background "#000000"))))
 ;; 文字種
 '(bold        ((t (:italic nil :bold nil :weight normal))))
 '(bold-italic ((t (:italic nil :bold nil :weight normal :slant normal))))
 ;; 選択範囲
 '(region ((t (:background "#3a3a3a"))))
 '(minibuffer-prompt ((t (:foreground "#cd0000"))))
 ;; モードライン
 '(mode-line           ((t (:foreground "#F8F8F2" :background "#000000" :box (:line-width 1 :color "#000000" :style released-button)))))
 '(mode-line-inactive  ((t (:foreground "#ffffff" :background "#7f7f7f" :box (:line-width 1 :color "#ffffff")))))
 '(mode-line-buffer-id ((t (:foreground nil :background nil))))
 ;; ハイライト
 '(highlight      ((t (:foreground "#000000" :background "#C4BE89"))))
 '(hl-line        ((t (:background "#293739"))))
 '(lazy-highlight ((t (:foreground "#000000" :background "#C4BE89"))))
 ;; font lock
 '(font-lock-preprocessor-face  ((t (:foreground "#8700ff"))))
 '(font-lock-function-name-face ((t (:foreground "#af005f"))))
 '(font-lock-type-face          ((t (:foreground "#af875f"))))
 '(font-lock-variable-name-face ((t (:foreground "#00cdcd"))))
 '(font-lock-string-face        ((t (:foreground "#808080"))))
 '(font-lock-keyword-face       ((t (:foreground "#005fd7"))))
 '(font-lock-builtin-face       ((t (:foreground "#0000ee"))))
 '(font-lock-constant-face      ((t (:foreground "#d78700"))))
 '(font-lock-error-face         ((t (:foreground "#cd0000"))))
 '(font-lock-warning-face       ((t (:foreground "#cd0000"))))
 '(font-lock-comment-face       ((t (:foreground "#005f00"))))
 ;; ispell
 ;;'(flyspell-duplicate ((t (:bold nil))))
 ;;'(flyspell-incorrect ((t (:bold nil))))
 ;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#ffffff"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#cd0000"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#00cd00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#0000cd"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#cdcd00"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#cd00cd"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#00ffff"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#af00ff"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#00d700"))))
 ;; Paren
 '(show-paren-match-face ((t (:foreground "#1B1D1E" :background "#949494"))))
 ;; Popup
 '(popup-face   ((t (:foreground "#000000" :background "#ffffff"))))
 ;; org
 '(org-todo                ((t (:foreground "#cd0000" :italic nil :bold nil :weight normal :slant normal))))
 '(org-agenda-date-weekend ((t (:foreground "#949494" :italic nil :bold nil :weight normal :slant normal))))
 '(org-agenda-date-today   ((t (:foreground "#af00ff" :italic nil :bold nil :weight normal :slant normal :underline t))))
 '(org-date                ((t (:foreground "#949494"))))
 ;; Css
 '(css-selector ((t (:foreground "#0000ee"))))
 '(css-property ((t (:foreground "#ff0000"))))
 ;; web
 '(web-mode-html-tag-bracket-face          ((t (:foreground "#0000ee"))))
 '(web-mode-html-tag-face                  ((t (:foreground "#0000ee"))))
 '(web-mode-doctype-face                   ((t (:foreground "#cd0000"))))
 '(web-mode-html-attr-name-face            ((t (:foreground "#cd0000"))))
 '(web-mode-current-element-highlight-face ((t (:foreground "#cd0000"))))
 '(web-mode-param-name-face                ((t (:foreground "#000000"))))
 ;; helm base
 '(helm-header           ((t (:foreground "#ffffff" :background "#ff8700"))))
 '(helm-candidate-number ((t (:foreground "#ffffff" :background "#005faf"))))
 '(helm-source-header    ((t (:foreground "#ffffff" :background "#949494" :bold nil))))
 '(helm-selection        ((t (:background "#cdcd00"))))
 '(helm-match            ((t (:foreground "#af0087"))))
 '(helm-grep-match       ((t (:foreground "#af0087"))))
 ;; helm filer
 '(helm-buffer-file                 ((t (:foreground "#ffffff"))))
 '(helm-buffer-directory            ((t (:foreground "#005fff"))))
 '(helm-ff-file                     ((t (:foreground "#ffffff"))))
 '(helm-ff-directory                ((t (:foreground "#005fff" :background nil))))
 '(helm-ff-dotted-directory         ((t (:foreground "#7f7f7f" :background nil))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "#7f7f7f" :background nil))))
 ;; helm m-x
 '(helm-M-x-key ((t (:foreground "#cd0000" :bold nil))))
 ;; helm-ls-git-ls
 '(helm-ls-git-modified-and-staged-face ((t (:foreground "#00cdcd" :bold nil))))
 '(helm-ls-git-modified-not-staged-face ((t (:foreground "#cd0000" :bold nil))))
 '(helm-ls-git-untracked-face           ((t (:foreground "#7f7f7f" :bold nil))))
 )
;; ###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(provide-theme 'kamome)
