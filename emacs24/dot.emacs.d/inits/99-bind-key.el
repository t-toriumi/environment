(use-package bind-key
  :ensure bind-key)

;; C- (reserve p,n, gtags t, migemo e)
(bind-key* "C-h"   'delete-backward-char)
(bind-key* "C-\\"  'undo)
(bind-key* "C-w"   'backward-kill-word-or-kill-region)
(bind-key* "C-v"   'half-page-up)
;;(bind-key  "C-l"   'recenter)
(bind-key* "C-1"   'start-kbd-macro)
(bind-key* "C-2"   'end-kbd-macro)
(bind-key* "C-3"   'call-last-kbd-macro)
(bind-key* "C-DEL" 'backward-kill-line)

;; M- (reserve x,y,z, gtags t, r, s)
(bind-key* "M-v"  'half-page-down)
(bind-key* "M-\\" 'redo)
(bind-key* "M-g"  'goto-line)
(bind-key* "M-p"  'repeat-complex-command)
(bind-key* "M-o"  'dmacro-exec)
(bind-key* "M-;"  'comment-dwim)
(bind-key* "M-2"  'er/expand-region)
(bind-key* "M-3"  'er/contract-region)
(bind-key* "M-q"  'next-buffer-with-skip*)

;; C-M-
(bind-key* "C-M-k" 'backward-kill-line)
(bind-key* "C-M-s" 'ispell-word)

;; C-c (reserve f,s.p,z)
(bind-key "C-c o" 'org-capture)
(bind-key "C-c a" 'org-agenda)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c y" 'helm-yas-complete)

;; C-x (reserve b)
(bind-key* "C-x p"   'prev-window)
(bind-key* "C-x SPC" 'cua-set-rectangle-mark)

;; C-c C- (php l)
(bind-key* "C-c C-r" 'query-replace)

;; C-c M-
(bind-key* "C-c M-r" 'replace-string)

;; C-x C-
(bind-key* "C-x C-h" 'highlight-symbol-at-point)
(bind-key* "C-x C-n" 'highlight-symbol-next)
(bind-key* "C-x C-p" 'highlight-symbol-prev)
(bind-key* "C-x C-e" 'highlight-symbol-query-replace)

;; ミニバッファ
(bind-key "\C-p" 'previous-history-element minibuffer-local-must-match-map)
(bind-key "\C-n" 'next-history-element     minibuffer-local-must-match-map)
(bind-key "\C-p" 'previous-history-element minibuffer-local-completion-map)
(bind-key "\C-n" 'next-history-element     minibuffer-local-completion-map)
(bind-key "\C-p" 'previous-history-element minibuffer-local-map)
(bind-key "\C-n" 'next-history-element     minibuffer-local-map)

;; helm
(bind-key* "M-x"   'helm-M-x)
(bind-key* "M-y"   'helm-show-kill-ring)
(bind-key* "M-z"   'helm-resume)
(bind-key* "C-x b" 'helm-buffers-list)
(bind-key* "C-c f" 'helm-for-files)
(bind-key* "C-c s" 'helm-swoop)
(bind-key* "C-c p" 'helm-ls-git-ls)
(bind-key* "C-c g" (lambda () (interactive) (setq current-prefix-arg '(4)) (call-interactively 'helm-do-grep)))

;; helm mini-buffer
(bind-key "C-h" 'delete-backward-char helm-map)
(bind-key "C-h" 'delete-backward-char helm-find-files-map)

;; helm select action
(bind-key "TAB" 'helm-execute-persistent-action helm-read-file-map)
(bind-key "TAB" 'helm-execute-persistent-action helm-find-files-map)
(bind-key "C-w" 'backward-kill-word             helm-read-file-map)
(bind-key "C-w" 'backward-kill-word             helm-find-files-map)

;; helm gtags
(bind-key "C-t" 'helm-gtags-find-tag    helm-gtags-mode-map)
(bind-key "M-t" 'helm-gtags-pop-stack   helm-gtags-mode-map)
(bind-key "M-r" 'helm-gtags-find-rtag   helm-gtags-mode-map)
(bind-key "M-s" 'helm-gtags-find-symbol helm-gtags-mode-map)

;; php-mode
(bind-key "C-c C-l" 'helm-imenu php-mode-map)

;; i-search
(bind-key "C-e" 'migemo-isearch-toggle-migemo         isearch-mode-map)
(bind-key "M-s" 'helm-swoop-from-isearch              isearch-mode-map)
(bind-key "M-s" 'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)

;; unset
(unbind-key "C-c C-f" php-mode-map)
(unbind-key "M-i" isearch-mode-map)
