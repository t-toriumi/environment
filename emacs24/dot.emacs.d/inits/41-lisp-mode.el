;; lisp-mode
(add-to-list 'auto-mode-alist '("\\.el\\'"  . emacs-lisp-mode))

;; hook
(defun lisp-mode-hooks ()
  ;; minor-mode
  (rainbow-delimiters-mode 1)
)
(add-hook 'lisp-mode-hook 'lisp-mode-hooks)

(defun emacs-lisp-mode-hooks ()
  ;; minor-mode
  (rainbow-delimiters-mode 1)
)
(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-mode-hooks)
