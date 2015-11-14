;; 現在位置から行頭まで一気に消す
(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line 0))

;; switch-to-buffer で 存在しないバッファ名を指定した時に新規バッファとして開けるようにする
(defun switch-to-buffer-extension (prompt)
  (interactive
   (list (read-buffer "Switch to buffer: " (other-buffer (current-buffer)))))
  (switch-to-buffer prompt))
(global-set-key "\C-xb" 'switch-to-buffer-extension)

;; リージョンがある場合はリージョンを、そうでない場合は前方の単語をkillする
(defun backward-kill-word-or-kill-region ()
  (interactive)
  (if (or (not transient-mark-mode) (region-active-p))
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; 画面分割時、前の画面を選択する
(defun prev-window ()
  (interactive)
  (other-window -1))

;; カーソルは画面内固定で半画面 scroll-up
(defun half-page-up()
  (interactive)
  (if(= (window-end) (point-max))
      (next-line (/ (window-height) 2))
    (let ((a (current-line)))
      (if(< a 1) (setq a 1))
      (scroll-up-command (/ (window-height) 2))
      (move-to-window-line a))))

;; カーソルは画面内固定で半画面 scroll-down
(defun half-page-down()
  (interactive)
  (if(= (window-start) 1)
      (next-line (/ (window-height) -2))
    (let ((a (current-line)))
      (scroll-down-command (/ (window-height) 2))
      (move-to-window-line a))))


;; ウィンドウ上でのカーソル位置(行数)を求める
(defun current-line()
  (cdr (nth 6 (posn-at-point))))

;; MacOSXでクリップボードと同期する
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(setq interprogram-cut-function   'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; バッファの*判定
(defun asterisked? (buf-name)
  (= 42 (car (string-to-list buf-name))))

;; 次のバッファへ
(defun next-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while (and (asterisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (next-buffer))))

;; 前のバッファへ
(defun previous-buffer-with-skip* ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (previous-buffer)
    (while (and (asterisked? (buffer-name))
                (not (string= current-buffer-name (buffer-name))))
      (previous-buffer))))
