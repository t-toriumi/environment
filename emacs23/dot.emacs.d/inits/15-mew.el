(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)

(setq mew-demo nil
      mew-fcc "+outbox"
      mew-name "Jun Nishikawa"
      mew-use-master-passwd t
      mew-prog-ssl "/usr/bin/stunnel4"
      mew-proto "%"
      exec-path (cons "/usr/bin" exec-path))

;; Optional setup (e.g. C-xm for sending a message):
(autoload 'mew-user-agent-compose "mew" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

;; (setq mew-config-alist
;;       '(("topaz2.3333@gmail.com"
;;          ("imap-server" . "imap.gmail.com")
;;          ("imap-user" . "topaz2.3333@gmail.com")
;;          ("mail-domain" . "gmail.com")
;;          ("imap-auth" . t)
;;          ("imap-ssl" . t)
;;          ("imap-ssl-port" . "993")
;;          ("smtp-auth" t)
;;          ("smtp-ssl" . t)
;;          ("smtp-ssl-port" . "465")
;;          ("smtp-user" . "topaz2.3333@gmail.com")
;;          ("smtp-server" . "smtp.gmail.com")
;;          ("fcc" . "%Sent")
;;          ("use-cached-passwd" . t)
;;          ("ssl-verify-level" . 0)
;;          )))
;; (setq mew-config-alist
;;       '(
;;         (default
;;           (name              "topaz2.3333@gmail.com")
;;           (user              "topaz2.3333@gmail.com")
;;           (mail-domain       "gmail.com")
;;           (ssl-verify-level  0)
;;           ;; setting for IMAP
;;           (proto             "%")
;;           (inbox-folder      "%inbox")     ;IMAPではinboxフォルダに固定されるので，意味が無い
;;           (imap-user         "topaz2.3333@gmail.com")
;;           (imap-server       "imap.gmail.com")
;;           (imap-ssl          t)
;;           (imap-ssl-port     993)
;;           (imap-delete       nil)
;;           (imap-auth         t)
;;           (imap-size         0)
;;           (imap-header-only  nil)
;;          ;; setting for SMTP
;;           (smtp-user         "topaz2.3333@gmail.com")
;;           (smtp-server       "smtp.gmail.com")
;;           (smtp-ssl          t)
;;           (use-smtp-auth     t)
;;           ;; (smtp-port         465)
;;           (smtp-ssl-port     587)
;;           (smtp-port         587)       ;サブミッションポート
;;           (use-submission    t)
;;           )))
