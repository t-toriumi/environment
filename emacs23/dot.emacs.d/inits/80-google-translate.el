(require 'google-translate)
(setq google-translate-default-source-language "en"
      google-translate-default-target-language "ja")

(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)
