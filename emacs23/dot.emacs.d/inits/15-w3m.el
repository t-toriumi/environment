(eval-after-load "w3m"
  '(progn
     (setq w3m-use-cookies t
           w3m-home-page "http://www.google.com/"
           w3m-search-default-engine "google-en"
           w3m-coding-system 'utf-8
           w3m-file-coding-system 'utf-8
           w3m-file-name-coding-system 'utf-8
           w3m-input-coding-system 'utf-8
           w3m-output-coding-system 'utf-8
           w3m-terminal-coding-system 'utf-8
           browse-url-generic-program (executable-find "w3m")
           browse-url-browser-function 'browse-url-generic)
     ))
