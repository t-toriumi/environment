(use-package smartchr)

(defun my-smartchr-braces ()
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "{\n}")
                  (indent-region beg (point))
                  (forward-line -1)
                  (indent-according-to-mode)
                  (goto-char (point-at-eol))
                  (setq end (save-excursion
                              (re-search-forward "[[:space:][:cntrl:]]+}" nil t))))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my-smartchr-semicolon ()
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (indent-according-to-mode)
                  (setq beg (point))
                  (insert ";")
                  (newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my-smartchr-arrow ()
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert " ->")
                  (coffee-newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))

(defun my-smartchr-fat-arrow ()
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert " =>")
                  (coffee-newline-and-indent)
                  (setq end (point)))
     :cleanup-fn (lambda ()
                   (delete-region beg end)))))


(defun smartchr-keybindings-ruby ()
  ;;(local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " " === " "=")))
  (local-set-key (kbd "~")  (smartchr '(" =~ " "~")))
  (local-set-key (kbd "+")  (smartchr '(" + " " += " "+")))
  (local-set-key (kbd "-")  (smartchr '("-" " - " " -= ")))
  (local-set-key (kbd ">")  (smartchr '(" > " " => " " >= " "->" ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " " !~ " "!")))
  (local-set-key (kbd "&")  (smartchr '(" & " " && " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" "**" " * ")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<")))
  (local-set-key (kbd "|")  (smartchr '("|`!!'|" " ||= " " || " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" "/`!!'/" " / " "// ")))
  (local-set-key (kbd "#")  (smartchr '("#{`!!'}" "#")))
  (local-set-key (kbd "(")  (smartchr '("()`!!'" "(" "(`!!')")))
  (local-set-key (kbd "[")  (smartchr '("[]`!!'" "[" "[`!!']")))
  (local-set-key (kbd "{")  (smartchr '("{`!!'}" "{|`!!'|  }" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-c/c++ ()
  ;;(local-set-key (kbd ";")  (smartchr '(my-smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " "=")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd ">")  (smartchr '(" > " " >> " " >= " "->" ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" " * " " *= ")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<`!!'>" "<")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" "// `!!'" " / " " /= ")))
  (local-set-key (kbd "(")  (smartchr '("()`!!'" "(" "(`!!')")))
  (local-set-key (kbd "[")  (smartchr '("[]`!!'" "[" "[`!!']")))
  (local-set-key (kbd "{")  (smartchr '(my-smartchr-braces "{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))

(defun smartchr-keybindings-php ()
  ;;(local-set-key (kbd ";")  (smartchr '(my-smartchr-semicolon ";")))
  (local-set-key (kbd ".")  (smartchr '("." " . " " .= ")))
  (local-set-key (kbd ",")  (smartchr '("," ", ")))
  (local-set-key (kbd "=")  (smartchr '("=" " = " " == " " === ")))
  (local-set-key (kbd "+")  (smartchr '("+" " + " "++" " += ")))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd ">")  (smartchr '(">" " > " "->" " => " " >= ")))
  (local-set-key (kbd "<")  (smartchr '("<" " < " " << " " <= ")))
  (local-set-key (kbd "%")  (smartchr '("%" " % " " %= ")))
  (local-set-key (kbd "!")  (smartchr '("!" " != ")))
  (local-set-key (kbd "&")  (smartchr '("&" " && " " & " " &= ")))
  (local-set-key (kbd "*")  (smartchr '("*" "**" " * " " *= ")))
  (local-set-key (kbd "|")  (smartchr '("|" " || " " |= ")))
  (local-set-key (kbd "/")  (smartchr '("/" "// `!!'" " / " " /= ")))
  (local-set-key (kbd "(")  (smartchr '("(" "(`!!')")))
  (local-set-key (kbd "[")  (smartchr '("[" "[`!!']")))
  (local-set-key (kbd "{")  (smartchr '(my-smartchr-braces "{" "{`!!'}")))
  (local-set-key (kbd "'")  (smartchr '("'" "'`!!''")))
  (local-set-key (kbd "\"") (smartchr '("\"" "\"`!!'\""))))

(defun smartchr-keybindings-js ()
  ;;(local-set-key (kbd ";")  (smartchr '(my-smartchr-semicolon ";")))
  (local-set-key (kbd ",")  (smartchr '(", " ",")))
  (local-set-key (kbd "=")  (smartchr '(" = " " == " " === " "=")))
  (local-set-key (kbd "+")  (smartchr '(" + " "++" " += " "+")))
  (local-set-key (kbd "-")  (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd ">")  (smartchr '(" > " " => " " >= " ">")))
  (local-set-key (kbd "%")  (smartchr '(" % " " %= " "%")))
  (local-set-key (kbd "!")  (smartchr '(" != " "!")))
  (local-set-key (kbd "&")  (smartchr '(" && " " & " " &= " "&")))
  (local-set-key (kbd "*")  (smartchr '("*" "**" " * " " *= ")))
  (local-set-key (kbd "<")  (smartchr '(" < " " << " " <= " "<")))
  (local-set-key (kbd "|")  (smartchr '(" || " " |= " "|")))
  (local-set-key (kbd "/")  (smartchr '("/" "// `!!'" " / " " /= ")))
  (local-set-key (kbd "(")  (smartchr '("()`!!'" "(" "(`!!')")))
  (local-set-key (kbd "[")  (smartchr '("[]`!!'" "[" "[`!!']")))
  (local-set-key (kbd "{")  (smartchr '(my-smartchr-braces "{`!!'}" "{")))
  (local-set-key (kbd "'")  (smartchr '("'`!!''" "'")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\""))))


(defun smartchr-keybindings-web-erb ()
  (local-set-key (kbd "<") (smartchr '("<%= `!!' %>" "<% `!!' %>" "<`!!'>" "<"))))

;; bind key
(add-hook 'php-mode-hook  'smartchr-keybindings-php)
(add-hook 'js2-mode-hook  'smartchr-keybindings-js)
