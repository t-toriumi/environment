(use-package auto-complete
  :ensure auto-complete)

(require 'auto-complete-config)

;; グローバルでauto-completeを利用
(global-auto-complete-mode t)

;; auto-complete
(ac-config-default)
(setq ac-use-menu-map t)

;; 自動で補完する
;;(setq ac-auto-start 3)
;; 手動で補完する
;; TABで補完開始(トリガーキー)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
