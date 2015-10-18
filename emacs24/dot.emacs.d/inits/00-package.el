;; パッケージ管理
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; packageの自動インストール。インストールされていなければ
;; 起動時に自動でインストールする。use-package ensureを使えない
;; 場合に利用し、基本はensureで対応していく。
(require 'cl)
(defvar installing-package-list
  '(
    ;; package
    use-package
    ))
(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))


;; 統一的なわかりやすい記述で書くことができるように
(require 'use-package)
