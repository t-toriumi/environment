(use-package helm
  :ensure helm)

(require 'helm)
(require 'helm-files)
(require 'helm-mode)

;; 既存のコマンドを Helm インターフェイスに置き換える
(helm-mode 1)
;;; 自動補完を無効
(custom-set-variables '(helm-ff-auto-update-initial-value nil))

;; helm-mode で無効にしたいコマンド
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil))

;; 一度に表示する最大候補数を増やす
(setq helm-candidate-number-limit 99999)
;; バッファ選択時の表示列
(setq helm-buffer-max-length 50)
