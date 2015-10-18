;; 日本語環
(set-language-environment       "Japanese")
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system           'utf-8)

;; ロードパス
(setq load-path
      (append '(
        "~/.elisp"
        "~/.emacs.d/site-lisp"
        "~/.emacs.d/themes"
        "~/.emacs.d/anything"
        "~/.emacs.d/web-mode"
        "~/.emacs.d/popup"
        "~/.emacs.d/auto-complete"
        "~/.emacs.d/auto-install")
       load-path))

;; 読み込み
(require 'redo)
(require 'popup)
(require 'php-mode)
(require 'anything-startup)
(require 'auto-complete-config)

;; バックアップの設定
(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying   t
      delete-old-versions t
      kept-new-versions   20
      kept-old-versions   20
      version-control     t)

;; デフォルト機能を使うときに有効に
;; これをONにすると期待しない機能がかなりONになり、色々と衝突するので注意
;;(require 'init-loader)
;;(setq init-loader-show-log-after-init nil)
;;(init-loader-load "~/.emacs.d/inits")

;; 自動インストール
;; 必要なときに有効にする。毎度Wiki更新するのがうざい。
;;(require 'auto-install)
;;(setq auto-install-directory "~/.emacs.d/auto-install/")
;;(auto-install-update-emacswiki-package-name t)
;;(auto-install-compatibility-setup)

;; 現在位置から行頭まで一気に消す
(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

;; switch-to-buffer で 存在しないバッファ名を指定した時に新規バッファとして開けるようにする。
(defun switch-to-buffer-extension (prompt)
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer (current-buffer)))))
  (switch-to-buffer prompt))
(global-set-key "\C-xb" 'switch-to-buffer-extension)

;; グローバルキー(C-)
(define-key global-map (kbd "C-h" )  'delete-backward-char)
(define-key global-map (kbd "C-;" )  'comment-dwim)
(define-key global-map (kbd "C-\\")  'undo)
(define-key global-map (kbd "C-_" )  'redo)

;; グローバルキー(M-)
(define-key global-map (kbd "M-g" )  'goto-line)
(define-key global-map (kbd "M-r" )  'query-replace)
(define-key global-map (kbd "M-p" )  'repeat-complex-command)

;; グローバルキー(C-M-)
(define-key global-map (kbd "C-M-k") 'backward-kill-line)
(define-key global-map (kbd "C-M-r") 'replace-string)

;; ミニバッファ
(define-key minibuffer-local-must-match-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-must-match-map "\C-n" 'next-history-element)
(define-key minibuffer-local-completion-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-completion-map "\C-n" 'next-history-element)
(define-key minibuffer-local-map "\C-p" 'previous-history-element)
(define-key minibuffer-local-map "\C-n" 'next-history-element)

;; スクロール
(setq next-screen-context-lines 2)

;; 起動画面を表示しない
(setq inhibit-startup-message t)
;; 折り返し表示
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
;; 補完等での横分割をしない
(setq-default split-width-threshold nil)
;; ファイル名の補完に大文字小文字を区別しない
(setq completion-ignore-case t)
;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)
;; メニュー非表示
(menu-bar-mode 0)
;; Yes/No 省略
(fset 'yes-or-no-p 'y-or-n-p)
;; 括弧
(show-paren-mode 1)
;; 現在行を目立たせる
(global-hl-line-mode)
(setq hl-line-face 'underline)
;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;; 改行コードを表示
(setq eol-mnemonic-dos  "(CRLF)")
(setq eol-mnemonic-mac  "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; タグジャンプ
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
       '(lambda ()
         (local-set-key "\C-t" 'gtags-find-tag)
         (local-set-key "\M-t" 'gtags-pop-stack)))

;; 行番号の書式
(setq linum-format "%5d ")
(add-hook 'linum-mode-hook
          '(lambda()
             (set-face-foreground 'linum "#ffffff")
             (set-face-background 'linum "#444444")))

;;フォントロック
(global-font-lock-mode t)
;; 色づけは最大限に
(setq font-lock-maximum-decoration t)

;; 日本語の空白表示
(require 'whitespace)
;; see whitespace.el for more details
(setq whitespace-style '(face trailing tabs empty tab-mark spaces space-mark))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\xBB ?\t] [?\\ ?\t])))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(set-face-foreground 'whitespace-tab   "#949494")
(set-face-background 'whitespace-tab   nil)
(set-face-foreground 'whitespace-space "#ff0000")
(set-face-background 'whitespace-space nil)
(set-face-bold-p     'whitespace-space t)
(global-whitespace-mode 1)

;; (require 'jaspace)
;; (setq jaspace-highlight-tabs ?^)
;; (setq jaspace-alternate-eol-string "↓\n")
;; (setq jaspace-alternate-jaspace-string "□")

;; develock
(defun recenter-and-fontify-buffer ()
  "comment..."
  (interactive)
  (recenter)
  (font-lock-fontify-buffer))

;; develock
;; http://www.jpl.org/elips/
(require 'develock nil t)
(if (featurep 'develock)
    (progn
      (setq develock-max-column-plist
            (list 'html-mode nil))
      (let ((elem (copy-sequence (assq 'message-mode develock-keywords-alist))))
        (setcar elem 'html-helper-mode)
        (setq develock-keywords-alist
              (cons elem (delq (assq 'html-helper-mode develock-keywords-alist)
                               develock-keywords-alist))))))

;; カラーテーマ
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'kamome t)
(enable-theme 'kamome)

;; autoload
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(autoload 'js2-mode "js2-mode" "Major mode for editing js  code." t)
(autoload 'css-mode "css-mode" "Major mode for editing css file." t)

;; 関連付け
(add-to-list 'auto-mode-alist '("\\.\\(js\\|json\\)$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.css$"             . css-mode))

;; デフォルト
(setq-default indent-tabs-mode               nil)
(setq-default tab-width                      4)
(setq-default c-basic-offset                 4)
(setq-default truncate-lines                 t)
(setq-default truncate-partial-width-windows t)

;; 行番号を表示するmode
(dolist (hook (list
           'emacs-lisp-mode-hook
           'c-mode-hook
           'php-mode-hook
           'css-mode-hook
           'js2-mode-hook))
  (add-hook hook (lambda () (linum-mode t))))

;; gtagsを利用するmode
(dolist (hook (list
           'gtags-select-mode-hook
           'c-mode-hook
           'php-mode-hook))
  (add-hook hook (lambda () (gtags-mode 1))))

;;; web-mode
(require 'web-mode)
;;; emacs 23以下の互換
(when (< emacs-major-version 24)
  (defalias 'prog-mode 'fundamental-mode))
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
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

;; auto-complete
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)

;; c-mode-hook
(add-hook 'c-mode-hook
          '(lambda ()
             (setq indent-tabs-mode               t)
             (setq tab-width                      4)
             (setq c-basic-offset                 4)
             (c-set-offset 'substatement-open'    0)))

;; php-mode-hook
(add-hook 'php-mode-hook
          '(lambda ()
             (setq indent-tabs-mode               nil)
             (setq tab-width                      4)
             (setq c-basic-offset                 4)
             (c-set-offset 'substatement-open'    0)
             (define-key php-mode-map (kbd "C-c C-l") 'anything-imenu)
             ;; php-completion
             (require 'php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "C-M-O") 'phpcmp-complete)
             (make-local-variable 'ac-sources)
             (setq ac-sources '(
                                ac-source-words-in-same-mode-buffers
                                ac-source-php-completion
                                ac-source-filename))))

;; javascript-mode-hook
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq indent-tabs-mode             nil)
             (setq tab-width                      4)
             (setq js2-basic-offset               4)
             (c-set-offset 'substatement-open'    0)))

;; css-mode-hook
(add-hook 'css-mode-hook
          '(lambda ()
             (setq indent-tabs-mode               nil)
             (setq tab-width                      2)
             (setq c-basic-offset                 2)
             (setq css-indent-offset              2)
             (c-set-offset 'substatement-open'    0)))

;; web-mode
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    4)
  (setq web-mode-java-offset   4)
  (setq web-mode-asp-offset    4))
(add-hook 'web-mode-hook 'web-mode-hook)

