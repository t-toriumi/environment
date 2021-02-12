(use-package auto-complete
  :ensure auto-complete)

(require 'auto-complete-config)

;; グローバルでauto-completeを利用
(global-auto-complete-mode t)

;; クイックヘルプを利用しない
(setq ac-use-quick-help nil)
;; 大文字・小文字を区別しない
(setq ac-ignore-case t)

;; 補完ソース
(setq ac-sources
      '(ac-source-yasnippet
       ac-source-abbrev
       ac-source-words-in-same-mode-buffers
       ac-source-dictionary
       ac-source-gtags))

;; 手動で補完せずTABで補完開始(トリガーキー)
(setq ac-auto-start 30)
(setq ac-dwim nil)
(ac-set-trigger-key "TAB")

;; 補完ウィンドウ内でのキー定義
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "M-/") 'ac-stop)

;; 候補の最大件数
(setq ac-candidate-max 20)

;; | フェイス名           | 説明                   |
;; |----------------------|------------------------|
;; | `ac-completion-face` | インライン補完の文字色 |
;; | `ac-candidate-face`  | 補完メニューの背景色   |
;; | `ac-selection-face`  | 補完メニューの選択色   |
(set-face-foreground 'ac-candidate-face  "#cccccc")
(set-face-background 'ac-candidate-face  "#4e4e4e")
(set-face-foreground 'ac-selection-face  "#ffffff")
(set-face-background 'ac-selection-face  "#587fff")
