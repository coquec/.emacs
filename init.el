; Añade los repositorios de paquetes de MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(when (< emacs-major-version 27)
  (package-initialize))

;; use-package hace más simple el fichero de configuración.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Usamos el modo AsciiDoc con los ficheros .adoc.
(add-to-list
 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; Habilitamos la comprobación ortográfica con flyspell en todos los
;; modos de texto y de programación.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Usamos un archivo temporal para guardar los cambios que se hagan
;; mediante el menú de configuración.  En la práctica, esto hace que
;; estos cambios no se guarden.  Toda la personalización debería
;; hacerse en este archivo.
(setq custom-file (make-temp-file "emacs-custom-" nil ".el"))
(load custom-file)

;; Deshabilita la página de inicio.
(setq inhibit-startup-screen t)

;; Deshabilita el sonido.
(setq visible-bell t)

;; Deshabilita la tool bar.
(tool-bar-mode -1)

;; Deshabilita la scroll bar.
(scroll-bar-mode -1)

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

;; Destacamos el paréntesis relacionado con el que está junto al
;; cursor.
(show-paren-mode t)

;; Habilitamos la historia de últimos ficheros abiertos.
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Guardamos la historia de últimos ficheros abiertos cada 5 min.
(run-at-time nil (* 5 60) 'recentf-save-list)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Habilitamos el ido-mode.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
    ;; This would override `fill-column' if it's an integer.
    (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Mostramos los números de línea en el margen izquierdo.
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Habilitamos el modo menor YASSnippet minor mode en adoc-mode.
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'adoc-mode-hook #'yas-minor-mode))

;; Habilitamos elpy al editar Python.
(use-package elpy
  :ensure t
  :defer t
  :init
  (elpy-enable))

;; Habilitamos EditorConfig para gestionar los valores por defecto en
;; cuanto a tabulaciones, finales de línea sin espacios, líneas en
;; blanco al final de los ficheros...
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Habilitamos magit para trabajar con Git desde Emacs.
(use-package magit)

;; Habilitamos hydra.
(use-package hydra)

;; Habilitamos indent-tools y usamos los bindings de hydra.
(use-package indent-tools
  :config
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))

;; Habilitamos las diapositivas en archivos org con org-tree-slide.
(use-package org-tree-slide)

;; Cargamos el tema por defecto
(load-theme 'wombat)
