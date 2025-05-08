;;; init.el --- My personal Emacs init file. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Coque Couto

;; Author: Coque Couto <coque.couto at gmail.com>
;; URL: https://github.com/coquec/.emacs

;; init.el is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; init.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; init.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; My personal init.el configuration file for Emacs.

;;; Code:
(require 'package)
(require 'use-package)

;; Add MELPA repositories.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)

;; Configure use-package default options.
(setopt use-package-always-ensure t     ; Install packages not yet installed.
        use-package-always-defer t      ; Load non-autoloaded functions only.
        )

;; Update packages automatically.
;; https://github.com/rranelli/auto-package-update.el
(use-package auto-package-update
  :config
  (setopt auto-package-update-delete-old-versions t)
  (setopt auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Load default theme.
(load-theme 'wombat)

;; Enable spelling with flyspell in text and programming modes.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(when (equal system-type 'windows-nt)
  (setopt ispell-dictionary "en_GB")
  (setopt ispell-local-dictionary-alist '(("en_GB"
                                           "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "[']"
                                           t
                                           ("-d" "en_GB")
                                           nil
                                           utf-8)
                                          ("en_US"
                                           "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "[']"
                                           t
                                           ("-d" "en_US")
                                           nil
                                           utf-8)
                                          ("sp"
                                           "[[:alpha:]]"
                                           "[^[:alpha:]]"
                                           "\\(?:\\`a\\`\\)"
                                           t
                                           ("-d" "es")
                                           nil utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

;; Use a temporary file to save configuration changes done via menu, and remove
;; it when emacs exits.  This is a way to disable the configuration menu.  All
;; the config must be done in the init.el file.
(setopt custom-file (make-temp-file "emacs-custom-" nil ".el"))
(add-hook 'kill-emacs-hook
          #'(lambda () (delete-file custom-file)))

;; Disable startup page.
(setopt inhibit-startup-screen t)

;; Disable sound.
(setq visible-bell t)

;; Disable tool bar.
(tool-bar-mode -1)

;; Disable scroll bar.
(scroll-bar-mode -1)

;; Disable menu bar.
(menu-bar-mode -1)

;; Disable message shown when files are saved.
(setq save-silently 1)

;; Enable repeat-mode.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Repeating.html
(repeat-mode)

;; Enable winner-mode, allowing to recover previous window layouts with C-c
;; <left> and C-c <right>.
(winner-mode)

;; Show the column number at the status bar.
(column-number-mode)

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

;; Adjust the width of the cursor to the character under it.  This helps to see
;; tabs.
(setq x-stretch-cursor 1)

;; Highlight the bracket matching the one next to the cursor.
(show-paren-mode t)

;; Enable saving last opened files history.
(recentf-mode 1)
(setopt recentf-max-menu-items 50)
(global-set-key
  (kbd "C-c c r") 'recentf-open-files)

;; Save last 5 open files history every 5 minutes.
(run-at-time nil (* 5 60) 'recentf-save-list)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable fido-mode.
(fido-mode)

;; Show line numbers at the left, with width enough space to hold the largest
;; number.  Highlight current line number with the lazy-highlight face.
(setopt display-line-numbers-grow-only t)
(setopt display-line-numbers-width-start 1)
(set-face-attribute 'line-number-current-line nil :inherit 'lazy-highlight)
(global-display-line-numbers-mode)

;; Frame of 80 columns wide plus the necessary for line numbers.
(when (display-graphic-p)
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

;; Enable additional movements while searching in Emacs 28.1 and later.
(setopt isearch-allow-motion t)

;; Switch the focus to help windows when they're opened.
(setopt help-window-select t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My own functions.

;; Joins all the lines of a paragraph in one.
(defun jcv/unfill-paragraph (&optional region)
  "Take a multi-line paragraph and make it into a single line of text.

If REGION is non-nil, apply the function to all the paragraphs in it."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key
  (kbd "C-c c q") 'jcv/unfill-paragraph)

;; Opens the directory with Emacs init file.
(defun jcv/find-init-directory ()
  "Opens the directory with Emacs init file."
  (interactive)
  (find-file user-emacs-directory))
(global-set-key
  (kbd "C-c c i") 'jcv/find-init-directory)

;; Functions for decoding Base64 strings in buffers.

(defun jcv/base64-decode-string (str &optional charset)
  "Decode a Base64 or Base64url encoded STR string.

Leave the result in UTF-8 by default, although CHARSET can be used to
choose another one.
 
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
  "Replace a Base64 or Base64url encoded region with its decoded version.

Replace the region enclosed by BEG and END with its decoded version.
Leave the mark at the beginning of the decoded text, and the point at
the end.

Leave the result in UTF-8 by default, although CHARSET can be used to
choose another one."
  (interactive "r")
  (let ((decoded-text
         (jcv/base64-decode-string (buffer-substring-no-properties
                                    beg end) charset)))
    (delete-region beg end)
    (goto-char beg)
    (set-mark beg)
    (insert decoded-text)))
 
(defun jcv/base64-decode-region-into-buffer (beg end &optional charset)
  "Decode a Base64 or Base64url encoded region to a temporary buffer.

Decode the region enclosed by BEG and END and append the result to the
temporary buffer *Base64 decode*.  Leave the mark in that buffer at the
beginning of the decoded text, and the point at the end.

Leave the result in UTF-8 by default, although CHARSET can be used to
choose another one."
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
  "Find the limits of the Base64 or Base64url string under the cursor.

Return a list with the limits of a Base64 or Base64url string under the
cursor, with the beginning at the first position and the end at the
second one."
  (list (+ 1 (re-search-backward "[^-_a-zA-Z0-9+/]"))
        ;; We consider all the '=' characters at the end of the string, as our
        ;; decode functions ignore them.
        (re-search-forward "[-_a-zA-Z0-9+/]+=*")))
 
(defun jcv/base64-decode-point-in-place ()
  "Decode in place the Base64 or Base64url string under the cursor.

Call `base64-decode-region' to do the job."
  (interactive)
  (save-excursion
    (apply 'jcv/base64-decode-region (jcv/base64-find-limits))))
 
(defun jcv/base64-decode-point-into-buffer ()
  "Decode in a temp buffer the Base64 or Base64url string under the cursor.

Call `base64-decode-region-into-buffer' to do the job."
  (interactive)
  (save-excursion
    (apply 'jcv/base64-decode-region-into-buffer (jcv/base64-find-limits))))
(global-set-key
  (kbd "C-c c 6") 'jcv/base64-decode-point-into-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional packages

;; Enable tree-sitter automatically for all the languages.
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Use corfu for in-buffer completion.  Use it with M-<tab> or with M-/.
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-preview-current t))

;; which-key shows the available keybindings while typing a prefix.
(use-package which-key
  :config
  (which-key-mode))

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
  (global-set-key (kbd "C-c c >") 'indent-tools-hydra/body))

;; YAML mode.
(use-package yaml-mode)

;; AsciiDoc mode.
(use-package adoc-mode)

;; Diminish.  Allow to use :diminish in use-package to hide minor modes.
(use-package diminish)

;; Paredit.  Edit parenthesis like a pro in lisp modes.
(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("C-c c [" . paredit-wrap-square)
         ("C-c c {" . paredit-wrap-curly))
  :diminish nil)

;; Rest.  Useful functions to call REST APIs.
(use-package rest
  :vc
  (:url "https://github.com/coquec/rest.git" :branch "main" :rev :newest))

;; IPv4.  Elisp functions to manipulate IP addresses.
(use-package ipv4
  :vc
  (:url "https://github.com/coquec/ipv4.git" :branch "main" :rev :newest))

;; I don't use the following packages in Windows.
(when (not (equal system-type 'windows-nt))
  ;; SLIME, for Common Lisp programming.
  (use-package slime)
  (setopt inferior-lisp-program "sbcl")

  ;; Geiser, for Scheme programming, only with guile for now.
  (use-package geiser-guile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode configuration.

(require 'org)
(require 'outline)

;; Enable presentations in org-mode with org-tree-slide.
(use-package org-tree-slide)

;; Avoid inheritance of the 'project' tag.  This allows to mark project tasks
;; explicitly and to have an agenda view with the current projects by typing
;; 'M-x org-agenda m project'.
(setopt org-tags-exclude-from-inheritance '("project"))

;; Record state changes in the default drawers "LOGBOOK".
(setopt org-log-into-drawer t)

;; Use different bullet types when demoting lists items.
(setopt org-list-demote-modify-bullet
        '(("-" . "+") ("+" . "-") ("*" . "-")))

;; Don't expand drawers when cycling visibility (e.g., with TAB or S-TAB).
(add-hook 'org-cycle-hook 'org-cycle-hide-drawers)

;; Active Babel languages.
(org-babel-do-load-languages 'org-babel-load-languages
                             '((shell . t)))

;; Copy to the clipboard the Nth header of the current path.
(defun jcv/get-org-header (n)
  "Return the header in the N position of the outline path.

If N is not specified, returns the second element, as if N=1."
  (nth (or n 1)
       (org-get-outline-path t)))

(defun jcv/org-header-to-kill-buffer (n)
  "Add to the kill ring the header in the N position of the outline path.

If N is not specified, add the second element, as if N=1."
  (interactive "p")
  (let ((header (jcv/get-org-header n)))
    (message header)
    (kill-new header)))

(defun jcv/org-drawer-contents (drawer-name)
  "Get the content of the drawer DRAWER-NAME inside the current heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((drawer-re (format "^[ \t]*:%s:[ \t]*$"
                             (regexp-quote drawer-name)))
          (end-drawer-re "^[ \t]*:END:[ \t]*$")
          (pos-next-heading (save-excursion (outline-next-heading))))
      (when (re-search-forward drawer-re pos-next-heading t)
        (let ((start (match-end 0)))
          (when (re-search-forward end-drawer-re pos-next-heading  t)
            (string-trim (buffer-substring-no-properties
                          start
                          (match-beginning 0)))))))))

(defun jcv/org-drawer-contents-in-hierarchy (drawer-name)
  "Return the contents of the drawer DRAWER-NAME in the heading hierarchy.

Look for the first drawer named DRAWER-NAME in the heading hierarchy,
from the current one upwards, and return its contents."
  (let ((content (jcv/org-drawer-contents drawer-name)))
    (or content
        (save-excursion
          (and (org-up-heading-safe)
               (jcv/org-drawer-contents-in-hierarchy drawer-name))))))

(defun jcv/org-get-participants (drawer-name)
  "Return a list of the participants' names of a meeting.

The list is obtained from the drawer named DRAWER-NAME of the current
task.  Participants must be list items with a checkbox.  Comments at the
end of each item (starting with \" - \"), will be ignored.

For the next examples, the function would return (\"Participant 2\",
\"Participant 3\"):

:PARTICIPANTS:
- [ ] Participant 1 - comment
- [X] Participant 2 - comment
- [X] Participant 3 - comment
:END:

:PARTICIPANTS:
- From other company:
  - [ ] Participant 1 - comment
  - [X] Participant 2 - comment
- From my company:
  - [X] Participant 3 - comment
:END:

If there are no participants with a check mark, return them all."
  (let ((participants-raw (jcv/org-drawer-contents-in-hierarchy drawer-name)))
    (and participants-raw
         (let ((participants-list
                (seq-filter (lambda (x)
                              (string-match "^[[:blank:]]*[-+*] \\[.\\] *" x))
                            (split-string participants-raw "\n"))))
           ;; Remove everything around the names.
           (mapcar
            ;; Use the non-greedy .*? to match the possible comments after the
            ;; name.
            (lambda (x)
              (replace-regexp-in-string
               "^[[:blank:]]*[-+*] \\[.\\] *\\(.*?\\)\\( +- +.*\\)?$"
               "\\1"
               x))
            ;; Apply the previous function to the names with a check mark (if
            ;; any), or to all of them.
            (or (seq-filter (lambda (x)
                              (string-match "^[[:blank:]]*[-+*] \\[[xX]\\] "
                                            x))
                            participants-list)
                participants-list))))))

(defun jcv/org-copy-participants ()
  "Copy to the clipboard the list of people attending a meeting.

Copy to the clipboard and prints in the minibuffer the list of a meeting
participants' names found by the `jcv/org-get-participants' function.
The list in printed as a string with semicolons between the names, in a
format ready to be pasted in an email."
  (interactive)
  (let ((result (mapconcat 'identity
                           (jcv/org-get-participants "PARTICIPANTS")
                           "; ")))
    (message result)
    (kill-new result)))

;; Customize org-mode key bindings.
(defun jcv/org-keybindings ()
  "Configure my keybindings in `org-mode'."
  (local-set-key (kbd "C-c c h")
                 'jcv/org-header-to-kill-buffer)
  (local-set-key (kbd "C-c c p")
                 'jcv/org-copy-participants))

;; Use my keybindings in org-mode.  I use a function instead of a lambda to
;; allow overriding it when changing the org-mode-hook.
(add-hook 'org-mode-hook 'jcv/org-keybindings)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load local configuration, which can hold sensitive data that should not be
;; included in the Git repo.

;; Load and run a local.el file in the same folder as init.el, if it exists.
(let ((local-init-file (concat user-emacs-directory "local.el")))
  (if (file-readable-p local-init-file)
      (load-file local-init-file)))
