;;; facemenu+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (facemenup-customize-face-at-point facemenup-customize-face-at-mouse
;;;;;;  facemenup-paste-to-face-fg-at-point facemenup-paste-to-face-bg-at-point
;;;;;;  facemenup-paste-to-face-fg-at-mouse facemenup-paste-to-face-bg-at-mouse
;;;;;;  facemenup-set-face-fg-RGB-hex-at-point facemenup-set-face-bg-RGB-hex-at-point
;;;;;;  facemenup-set-face-fg-RGB-hex-at-mouse facemenup-set-face-bg-RGB-hex-at-mouse
;;;;;;  facemenup-set-face-fg-RGB-at-point facemenup-set-face-bg-RGB-at-point
;;;;;;  facemenup-set-face-fg-RGB-at-mouse facemenup-set-face-bg-RGB-at-mouse
;;;;;;  facemenup-face-fg-restore facemenup-face-bg-restore facemenup-describe-text-properties-at-mouse
;;;;;;  facemenup-help facemenup-palette-update-while-editing-flag)
;;;;;;  "facemenu+" "facemenu+.el" (21151 25086 986166 801000))
;;; Generated autoloads from facemenu+.el

(defvar facemenup-palette-update-while-editing-flag t "\
*Non-nil means update the face whose color you're editing in the palette.
The face is updated automatically each time you change the palette's
current color, which is typically when you hit `RET' or click
`mouse-2'.  If nil, then the face is updated only when you exit the
palette using `x'.")

(custom-autoload 'facemenup-palette-update-while-editing-flag "facemenu+" t)

(defconst facemenup-highlight-menu (easy-menu-create-menu "Highlight" (append '(["Highlight Region/Buffer" hlt-highlight-region t] ["Highlight Regexp in Region/Buffer..." hlt-highlight-regexp-region t] ["Highlight Regexp to End" hlt-highlight-regexp-to-end t] ["Unhighlight Region/Buffer" hlt-unhighlight-region t] ["Unhighlight Region/Buffer for Face" hlt-unhighlight-region-for-face t] "--" ["Copy Text Properties" hlt-copy-props t] ["Paste Text Properties to Region" hlt-yank-props (and (facemenup-nonempty-region-p) (not buffer-read-only) hlt-copied-props)] "--" ["Highlighter Pen" hlt-highlighter-mouse t] ["Eraser" hlt-eraser-mouse t]) (and (fboundp 'hlt-show-only) '("--" ["Hide Only Faces..." hlt-hide-only t] ["Show Only Faces..." hlt-show-only t] ["Show Faces..." hlt-show t] ["Hide Faces..." hlt-hide t] "--")) '(["Choose Highlighting Face" hlt-choose-default-face t] ["Replace Highlighting Face in Region/Buffer" hlt-replace-highlight-face t] ["Toggle Using Overlays for Highlighting" hlt-toggle-use-overlays-flag t]) (and (fboundp 'hlt-toggle-act-on-any-face-flag) '(["Toggle Highlighting Arbritrary Faces" hlt-toggle-act-on-any-face-flag t])) (and (fboundp 'hlt-next-highlight) '("--" ["Go To Next Highlight" hlt-next-highlight t] ["Go To Previous Highlight" hlt-previous-highlight t])))) "\
Highlight submenu of Text Properties menu.")

(autoload 'facemenup-help "facemenu+" "\
Open the Emacs manual to help about formatted text.

\(fn EVENT)" t nil)

(autoload 'facemenup-describe-text-properties-at-mouse "facemenu+" "\
Describe text properties of character under the mouse pointer.

\(fn EVENT)" t nil)

(autoload 'facemenup-face-bg-restore "facemenu+" "\
Restore background of last face changed by face menu to last color.
This is not an undo: It always restores the previous color as
the background of the last face changed.
This does not work for face changes made by Do Re Mi.

\(fn)" t nil)

(autoload 'facemenup-face-fg-restore "facemenu+" "\
Restore foreground of last face changed by face menu to last color.
This is not an undo: It always restores the previous color as
the foreground of the last face changed.
This does not work for face changes made by Do Re Mi.

\(fn)" t nil)

(autoload 'facemenup-set-face-bg-RGB-at-mouse "facemenu+" "\
Set RGB of background of face at character under the mouse pointer.
RGB is specified in decimal.

\(fn EVENT)" t nil)

(autoload 'facemenup-set-face-fg-RGB-at-mouse "facemenu+" "\
Set RGB of foreground of face at character under the mouse pointer.
RGB is specified in decimal.

\(fn EVENT)" t nil)

(autoload 'facemenup-set-face-bg-RGB-at-point "facemenu+" "\
Set RGB of background of face at character following cursor (point).
RGB is specified in decimal, from 0 to 255.

\(fn)" t nil)

(autoload 'facemenup-set-face-fg-RGB-at-point "facemenu+" "\
Set RGB of foreground of face at character following cursor (point).
RGB is specified in decimal, from 0 to 255.

\(fn)" t nil)

(autoload 'facemenup-set-face-bg-RGB-hex-at-mouse "facemenu+" "\
Set RGB of background of face at character under the mouse pointer.
RGB is specified in hexadecimal, from 0 to FFFF.

\(fn EVENT)" t nil)

(autoload 'facemenup-set-face-fg-RGB-hex-at-mouse "facemenu+" "\
Set RGB of foreground of face at character under the mouse pointer.
RGB is specified in hexadecimal, from 0 to FFFF.

\(fn EVENT)" t nil)

(autoload 'facemenup-set-face-bg-RGB-hex-at-point "facemenu+" "\
Set RGB of background of face at character following cursor (point).
RGB is specified in hexadecimal, from 0 to FFFF.

\(fn)" t nil)

(autoload 'facemenup-set-face-fg-RGB-hex-at-point "facemenu+" "\
Set RGB of foreground of face at character following cursor (point).
RGB is specified in hexadecimal, from 0 to FFFF.

\(fn)" t nil)

(autoload 'facemenup-paste-to-face-bg-at-mouse "facemenu+" "\
Paste last color copied to background of face under mouse.
The last color copied is in `eyedrop-last-picked-color'.

\(fn EVENT)" t nil)

(autoload 'facemenup-paste-to-face-fg-at-mouse "facemenu+" "\
Paste last color copied to foreground of face under mouse.
The last color copied is in `eyedrop-last-picked-color'.

\(fn EVENT)" t nil)

(autoload 'facemenup-paste-to-face-bg-at-point "facemenu+" "\
Paste last color copied to background of face at cursor (point).
The last color copied is in `eyedrop-last-picked-color'.

\(fn)" t nil)

(autoload 'facemenup-paste-to-face-fg-at-point "facemenu+" "\
Paste last color copied to foreground of face at cursor (point).
The last color copied is in `eyedrop-last-picked-color'.

\(fn)" t nil)

(autoload 'facemenup-customize-face-at-mouse "facemenu+" "\
Customize the face used at character under the mouse pointer.

\(fn EVENT)" t nil)

(autoload 'facemenup-customize-face-at-point "facemenu+" "\
Customize the face used at character following cursor (point).

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("facemenu+-pkg.el") (21151 25087 6627
;;;;;;  651000))

;;;***

(provide 'facemenu+-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; facemenu+-autoloads.el ends here
