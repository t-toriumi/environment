;; aspell manual
;;
;; 0, 1, ...
;;    0, 1, ... 番目の候補で置き換える。
;; SPC
;;    単語をスキップ。
;; r
;;    置換後の単語のつづりの入力を求められるので、入力して Return。
;; a
;;    修正せずに、この編集セッションに限って正しいものとして扱う。
;; i
;;    この単語を個人辞書ファイル（ホームディレクトリにある）に登録し、以後は正しいものとして扱われる。
;; C-g
;;    スペルチェックを中断する。「C-u M-$」で再開できる。
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

;; コメント領域のところだけスペルチェック対象
(mapc
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   ;;php-mode-hook
   ))
;; バッファ全体がスペルチェック対象
(mapc
 (lambda (hook)
   (add-hook hook
             '(lambda () (flyspell-mode 1))))
 '(
   ))
