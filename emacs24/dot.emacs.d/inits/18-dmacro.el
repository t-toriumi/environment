;; dmacro
(defun dmacro-exec ()
  (interactive)
  (let ((*dmacro-key* (this-single-command-keys)))
    (use-package dmacro)
    ;; dmacro-exec is overriden here
    (call-interactively 'dmacro-exec)))
