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
;; フォントロック
(global-font-lock-mode t)
;; 色づけは最大限に
(setq font-lock-maximum-decoration t)
;; リージョンを反転しない
;;(setq transient-mark-mode nil)
