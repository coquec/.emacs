; Add MELPA and GNU repositories.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
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

;; Show the column number at the status bar.
(setq column-number-mode t)

;; Use the current buffer name as the frame tittle.
(setq frame-title-format "%b")

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
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Save last 5 open files history every 5 minutes.
(run-at-time nil (* 5 60) 'recentf-save-list)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Enable ido-mode.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; This function joins all the lines of a paragraph in one.
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
    ;; This would override `fill-column' if it's an integer.
    (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(global-set-key
  (kbd "C-c q") 'unfill-paragraph)

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

;; Shortcut to copy the name of the current buffer to the kill-ring.
(global-set-key
  (kbd "C-c w")
  (lambda () (interactive) (kill-new (file-name-nondirectory buffer-file-name))))

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
