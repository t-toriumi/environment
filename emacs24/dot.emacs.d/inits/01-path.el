; ロードパス
(add-to-list 'load-path "~/.emacs.d/elisp")
(add-to-list 'load-path "~/.emacs.d/elisp/anything")
(add-to-list 'load-path "~/.emacs.d/elisp/work")
;;(add-to-list 'load-path "~/.emacs.d/elisp/themes")
(add-to-list 'load-path "~/.emacs.d/elisp/auto-install")
(add-to-list 'load-path "~/.emacs.d/elisp/php-completion")

;; バックアップ
(setq backup-directory-alist `(("." . "~/.saves"))
      backup-by-copying   t
      delete-old-versions t
      kept-new-versions   20
      kept-old-versions   20
      version-control     t)
