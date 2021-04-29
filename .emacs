; Añade los repositorios de paquetes de MELPA.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

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
 '(js-indent-level 3)
 '(package-selected-packages
   '(indent-tools paredit adoc-mode magit use-package json-mode terraform-mode markdown-mode)))

;; Deshabilita el sonido.
(setq visible-bell t)

;; Deshabilita la tool bar.
(tool-bar-mode -1)

;; Utiliza espacios en vez de tabuladores.
(setq-default indent-tabs-mode nil)

;; Muestra el número de columna en la barra de estado.
(setq column-number-mode t)

;; Usa el nombre del buffer como título del marco.
(setq frame-title-format "%b")

;; Fijamos la tabulación por defecto a cuatro espacios.
(setq-default tab-width 4)

;; Fijamos la tabulación del buffer actual a cuatro espacios.
(setq tab-width 4)

;; Fijamos las paradas del tabulador cada 4 caracteres.
(setq tab-stop-list (number-sequence 4 200 4))

;; Habilitamos la historia de últimos ficheros abiertos.
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Guardamos la historia de últimos ficheros abiertos cada 4 min.
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

;; Enable ido-mode
(require 'ido)
(ido-mode t)

;; Enable line numbers
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Enable YASSnippet minor mode in adoc-mode
(require 'yasnippet)
(yas-reload-all)
(add-hook 'adoc-mode-hook #'yas-minor-mode)

;; Enable highlight matching parentheses.
(show-paren-mode t)

;; Enable elpy for Python editing
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable))

;; Enable EditorConfig.
(require 'editorconfig)
(editorconfig-mode 1)

;; Enable magit.
(require 'magit)

;; Enable hydra.
(require 'hydra)

;; Enable indent-tools and use hydra bindings
(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)

;; Enable org-tree-slide.
(require 'org-tree-slide)
