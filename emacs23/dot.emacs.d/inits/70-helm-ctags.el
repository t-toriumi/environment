(require 'helm-etags+)
(require 'ctags-update)

(setq tags-table-list '("TAGS"))
;; (global-set-key "\M-." 'helm-etags+-select)
;; (global-set-key "\M-*" 'helm-etags+-history)
;; (global-set-key "\M-," 'helm-etags+-history-action-go-back)
;; (global-set-key "\M-/" 'helm-etags+-history-action-go-forward)

(setq ctags-update-other-options
  '(
   "--fields=+iaSt"
   "--extra=+q"
   "−−c++−kinds=+p"
   "--exclude='*.elc'"
   "--exclude='*.class'"
   "--exclude='*.min.js'"
   "--exclude='node_modules'"
   "--exclude='.git'"
   "--exclude='.svn'"
   "--exclude='SCCS'"
   "--exclude='RCS'"
   "--exclude='CVS'"
   "--exclude='EIFGEN'"))

;; (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
;; (add-hook 'c-mode-common-hook 'turn-on-ctags-auto-update-mode)
;; (add-hook 'emacs-lisp-mode-hook 'turn-on-ctags-auto-update-mode)

;; (autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
;; (global-set-key "\C-cE" 'ctags-update)
