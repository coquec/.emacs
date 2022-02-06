; Añade los repositorios de paquetes de MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(when (< emacs-major-version 27)
  (package-initialize))

;; use-package to simplify the config file
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Pone el modo AsciiDoc al abrir los ficheros .adoc.
(add-to-list
  'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes '(wombat))
 '(js-indent-level 3))

;; Deshabilita el mensaje de arranque.
(setq inhibit-startup-message t)

;; Deshabilita el sonido.
(setq visible-bell t)

;; Deshabilita la tool bar.
(tool-bar-mode -1)

;; Deshabilita la scroll bar.
(toggle-scroll-bar -1)

;; Muestra el número de columna en la barra de estado.
(setq column-number-mode t)

;; Usa el nombre del buffer como título del marco.
(setq frame-title-format "%b")

;; Utiliza espacios en vez de tabuladores.
(setq-default indent-tabs-mode nil)

;; Fijamos la tabulación por defecto a cuatro espacios.
(setq-default tab-width 4)

;; Fijamos la tabulación del buffer actual a cuatro espacios.
(setq tab-width 4)

;; Fijamos las paradas del tabulador cada 4 caracteres.
(setq tab-stop-list (number-sequence 4 200 4))

;; Enable highlight matching parentheses.
(show-paren-mode t)

;; Habilitamos la historia de últimos ficheros abiertos.
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Guardamos la historia de últimos ficheros abiertos cada 5 min.
(run-at-time nil (* 5 60) 'recentf-save-list)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
    ;; This would override `fill-column' if it's an integer.
    (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Enable line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Enable ido-mode
(use-package ido
  :ensure t
  :config
  (ido-mode t))

;; Enable YASSnippet minor mode in adoc-mode
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'adoc-mode-hook #'yas-minor-mode))

;; Enable elpy for Python editing
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable))

;; Enable EditorConfig.
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Enable magit.
(use-package magit)

;; Enable hydra.
(use-package hydra)

;; Enable indent-tools and use hydra bindings
(use-package indent-tools
  :config
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))

;; Enable org-tree-slide.
(use-package org-tree-slide)
