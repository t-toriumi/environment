(require 'pos-tip)

(defadvice popup-menu-show-quick-help
  (around pos-tip-popup-menu-show-quick-help () activate)
  "Show quick help using `pos-tip-show'."
  (if (eq window-system 'x)
      (let ((doc (popup-menu-document
                  menu (or item
                           (popup-selected-item menu)))))
        (when (stringp doc)
          (pos-tip-show doc nil
                        (if (popup-hidden-p menu)
                            (or (plist-get args :point)
                                (point))
                          (overlay-end (popup-line-overlay
                                        menu (+ (popup-offset menu)
                                                (popup-selected-line menu)))))
                        nil 0)
          nil))
    ad-do-it))

;; ;; 2 solution2
;; (require 'popup-pos-tip)
;;  (defadvice popup-tip
;;    (around popup-pos-tip-wrapper (string &rest args) activate)
;;    (if (eq window-system 'x)
;;        (apply 'popup-pos-tip string args)
;;      ad-do-it))

;; ;; 3
;;  (require 'sdic-inline-pos-tip)

;;  ;; *Change the following lines according to your environment*
;;  (setq sdic-inline-eiwa-dictionary "/usr/share/dict/gene.sdic")
;;  (setq sdic-inline-waei-dictionary "/usr/share/dict/jedict.sdic")

;;  ;; The following is optional. Uncomment if necessary.
;;  ;; (mapc (lambda (mode)
;;  ;;         (add-to-list 'sdic-inline-enable-modes mode))
;;  ;;       '(help-mode Info-mode))
;;  ;; (mapc (lambda (face)
;;  ;;         (add-to-list 'sdic-inline-enable-faces face))
;;  ;;       '(font-lock-doc-face))

;;  (setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
;;  (setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

;;  (define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)

;;  (sdic-inline-mode 1)
