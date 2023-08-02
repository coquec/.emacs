; Add MELPA and GNU repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 27)
  (package-initialize))

;; Load default theme.
(load-theme 'wombat)

;; Enable spelling with flyspell in text and programming modes.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Use a temporary file to save configuration changes done via menu.
;; This is a way to disable the configuration menu.  All the config
;; must be done in this file.
(setq custom-file (make-temp-file "emacs-custom-" nil ".el"))
(load custom-file)

;; Disable startup page.
(setq inhibit-startup-screen t)

;; Disable sound.
(setq visible-bell t)

;; Disable tool bar.
(tool-bar-mode -1)

;; Disable scroll bar.
(scroll-bar-mode -1)

;; Disable menu bar.
(menu-bar-mode -1)

;; Enable repeat-mode.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Repeating.html
(repeat-mode)

;; Show the column number at the status bar.
(setq column-number-mode t)

;; Use the current buffer name as the frame tittle.
(setq frame-title-format "%b")

;; Don't adjust frame size to row/columns multiples.
(setq frame-resize-pixelwise t)

;; Use spaces instead of tab characters.
(setq-default indent-tabs-mode nil)

;; Default tabs each 4 characters.
(setq-default tab-width 4)

;; Current buffer tabs each 4 characters.
(setq tab-width 4)

;; Tab stops each 4 characters.
(setq tab-stop-list (number-sequence 4 200 4))

;; Highlight the bracket corresponding to the one next to the cursor.
(show-paren-mode t)

;; Enable saving last opened files history.
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key
  (kbd "C-c c r") 'recentf-open-files)

;; Save last 5 open files history every 5 minutes.
(run-at-time nil (* 5 60) 'recentf-save-list)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable ido-mode.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Show line numbers at the left.
(global-display-line-numbers-mode)

;; Frame of 80 columns wide plus the necessary for line numbers.
(if (display-graphic-p)
  (setq default-frame-alist
    '((width . 87)
      (height . 35))))

;; Default end of line at column 79.
(setq-default fill-column 79)

;; Rebind M-/ to hippie expand
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Rebind C-x C-b to ibuffer
;; Recommended in Mastering Emacs.
(global-set-key [remap list-buffers] 'ibuffer)

;; Shortcut to copy the name of the current buffer to the kill-ring.
(global-set-key
  (kbd "C-c c w")
  (lambda () (interactive) (kill-new (file-name-nondirectory buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My own functions.

;; This function joins all the lines of a paragraph in one.
(defun jcv/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
    ;; This would override `fill-column' if it's an integer.
    (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key
  (kbd "C-c c q") 'jcv/unfill-paragraph)

;; Functions for decoding Base64 strings in buffers.

(defun jcv/base64-decode-string (str &optional charset)
  "Decode a Base64 or Base64url encoded string, coding the result
in UTF-8 by default, although CHARSET can be used to choose
another one.
 
Base64 strings are converted to Base64url format before decoding,
so there can be any number of '=' characters at the end."
  (let ((charset (or charset 'utf-8)))
    (decode-coding-string
     (base64-decode-string
      (string-replace "+" "-"
                      (string-replace "/" "_"
                                      (string-replace "=" "" str))) t)
     charset)))
 
(defun jcv/base64-decode-region (beg end &optional charset)
  "Replaces a Base64 or Base64url text enclosed by the region with
its decoded version.  Leaves the mark at the beginning of the
decoded text, and the point at the end."
  (interactive "r")
  (let ((decoded-text
         (jcv/base64-decode-string (buffer-substring-no-properties
                                    beg end) charset)))
    (delete-region beg end)
    (goto-char beg)
    (set-mark beg)
    (insert decoded-text)))
 
(defun jcv/base64-decode-region-into-buffer (beg end &optional charset)
  "Decode a Base64 or Base64url encoded region and appends the
result to the temporary buffer *Base64 decode*.  Leaves the mark
in that buffer at the beginning of the decoded text, and the
point at the end."
  (interactive "r")
  (let* ((output-buffer (get-buffer-create "*Base64 decode*"))
         (encoded-text (buffer-substring-no-properties beg end))
         (decoded-text (jcv/base64-decode-string encoded-text charset)))
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (insert encoded-text "\n")
      (set-mark (point-max))
      (insert decoded-text "\n"))
    (switch-to-buffer-other-window output-buffer)
    (set-window-point
     (get-buffer-window output-buffer)
     (- (point-max) 1))))
 
(defun jcv/base64-find-limits ()
  "Returns a list with the limits of a Base64 or Base64url string
under the cursor, with the beginning at the first position and
the end at the second one."
  (list (+ 1 (re-search-backward "[^-_a-zA-Z0-9+/]"))
        ;; We consider all the '=' characters at the end of the string, as our
        ;; decode functions ignore them.
        (re-search-forward "[-_a-zA-Z0-9+/]+=*")))
 
(defun jcv/base64-decode-point-in-place ()
  "Decode the Base64 or Base64url string under the cursor calling
`base64-decode-region', leaving the result in the same buffer."
  (interactive)
  (save-excursion
    (apply 'jcv/base64-decode-region (jcv/base64-find-limits))))
 
(defun jcv/base64-decode-point-into-buffer ()
  "Decode the Base64 or Base64url string under the cursor calling
`base64-decode-region-into-buffer', leaving the result in a
temporary buffer."
  (interactive)
  (save-excursion
    (apply 'jcv/base64-decode-region-into-buffer (jcv/base64-find-limits))))
(global-set-key
  (kbd "C-c c 6") 'jcv/base64-decode-point-into-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional packages

;; use-package macro simplifies the config file.
;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install all the packets not yet installed.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Update packages automatically.
;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Enable YASSnippet minor mode in adoc-mode.
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'adoc-mode-hook #'yas-minor-mode))

;; EditorConfig package allows to use .editorconfig files to set default
;; values for tabs, end of lines, emptylines...
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; magit package to use Git from Emacs.
(use-package magit)

;; Use hydra.
(use-package hydra)

;; Enable indent-tools and use hydra bindings.
(use-package indent-tools
  :config
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))

;; YAML mode.
(use-package yaml-mode)

;; AsciiDoc mode.
(use-package adoc-mode)

;; SLIME, for Common Lisp programming.
;; I don't use this one in Windows.
(when (not (equal system-type 'windows-nt))
  (use-package slime)
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode configuration.

;; Enable presentations in org-mode with org-tree-slide.
(use-package org-tree-slide)

;; Avoid inheritance of the 'project' tag.  This allows to mark project tasks
;; explicitly and to have an agenda view with the current projects by typing
;; 'M-x org-agenda m project'.
(setq org-tags-exclude-from-inheritance '("project"))

;; Record state changes in the default drawers "LOGBOOK".
(setq org-log-into-drawer t)

;; Copy to the clipboard the second top-most header of the current path, and
;; assign a key binding to it.
(defun jcv/org-project-to-kill-buffer ()
  "Places in a new kill buffer the header of the current
project (we suppose it's the second element in the outline
path)."
  (interactive)
  (let ((project (nth 1 (org-get-outline-path))))
    (message project)
    (kill-new project)))
(add-hook
 'org-mode-hook
 (lambda () (local-set-key (kbd "C-c c p")
                                'jcv/org-project-to-kill-buffer)))
