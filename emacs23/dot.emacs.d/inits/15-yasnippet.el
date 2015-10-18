(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa/yasnippet-20131203.720/"))
(require 'yasnippet)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t)

;; ;; ポップアップを dropdown-list に
;; (require 'dropdown-list)
;; (setq yas/text-popup-function #'yas/dropdown-list-popup-for-template)

;; ;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; ;; (setqだとtermなどで干渉問題ありでした)
;; ;; もちろんTAB以外でもOK 例えば "C-;"とか
;; (custom-set-variables '(yas-trigger-key "TAB"))

;; ;; ちなみに，ググったらよく出てくる (setq yas/trigger-key (kbd "SPC")) という設定は Emacs 23.2.1 では動きません
;; ;; (setq yas/trigger-key "SPC") だと動きます．
;; ;; (setq yas/trigger-key "TAB")
;; (setq yas/next-field-key "TAB")
;; (setq yas/prev-field-key "S-TAB")

;; (global-unset-key (kbd "C-x i"))
;; (define-key global-map (kbd "C-x i i") 'yas/insert-snippet)
;; (define-key global-map (kbd "C-x i f") 'yas/find-snippets)
;; (define-key global-map (kbd "C-x i n") 'yas/new-snippet)
;; (define-key global-map (kbd "C-x i v") 'yas/visit-snippet-file)
;; (define-key global-map (kbd "C-x i e") 'yas/expand)

;; ;; コメントやリテラルではスニペットを展開しない
;; ;; http://d.hatena.ne.jp/gan2/20080402/1207135480
;; (setq yas-buffer-local-condition
;;       '(or (not (or (string= "font-lock-comment-face"
;;                              (get-char-property (point) 'face))
;;                     (string= "font-lock-string-face"
;;                              (get-char-property (point) 'face))))
;;            '(require-snippet-condition . force-in-comment)))

;; ;; yasnippet展開中はflymakeを無効にする
(defvar flymake-is-active-flag nil)
(defadvice yas/expand-snippet
  (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
  (setq flymake-is-active-flag
        (or flymake-is-active-flag
            (assoc-default 'flymake-mode (buffer-local-variables))))
  (when flymake-is-active-flag
    (flymake-mode-off)))

(defun yas/after-exit-snippet-hook-for-active-flymake ()
  (when flymake-is-active-flag
    (flymake-mode-on)
    (setq flymake-is-active-flag nil)))
(add-hook 'yas/after-exit-snippet-hook 'yas/after-exit-snippet-hook-for-active-flymake)

;; (add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; (require 'yasnippet)
;; (yas/initialize)
;; (setq yas/root-directory "~/.emacs.d/snippets")
;; (yas-load-directory yas/root-directory)
;; (require 'dropdown-list)
;; (setq yas-prompt-functions '(yas/dropdown-prompt
;;                              yas/ido-prompt
;;                              yas/completing-prompt))

;;(require 'yasnippet-bundle)
