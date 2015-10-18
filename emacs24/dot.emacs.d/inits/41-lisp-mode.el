;; lisp-mode
(add-to-list 'auto-mode-alist '("\\.el\\'"  . emacs-lisp-mode))

;; hook
(defun lisp-mode-hooks ()
  ;; minor-mode
  (rainbow-delimiters-mode 1)
)

(add-hook 'lisp-mode-hook 'lisp-mode-hooks)
