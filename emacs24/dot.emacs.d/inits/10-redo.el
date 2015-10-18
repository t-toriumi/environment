(use-package redo+
  :ensure redo+)

;; 過去の undo を redo しない
(setq undo-no-redo t)
;; 大量のundoに耐える
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
