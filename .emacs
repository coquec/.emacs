
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Show the column number in the status bar
(setq column-number-mode t)

;; Use the buffer name as the frame tittle
(setq frame-title-format "%b")
