;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

; Añade los repositorios de paquetes de MELPA.
(require 'package)
(add-to-list
  'package-archives
  ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
  '("melpa" . "http://melpa.milkbox.net/packages/")
  t)
(package-initialize)

;; Pone el modo AsciiDoc al abrir los ficheros .adoc.
(add-to-list
  'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; Deshabilita el sonido.
(setq visible-bell t)

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

;; Mostramos el número de columna en la barra de estado.
(setq column-number-mode t)

;; Fijamos las paradas del tabulador cada 3 caracteres.
(setq tab-stop-list (number-sequence 4 200 4))

;; Habilitamos la historia de últimos ficheros abiertos.
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Guardamos la historia de últimos ficheros abiertos cada 4 min.
(run-at-time nil (* 5 60) 'recentf-save-list)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
    [default default default italic underline success warning error])
  '(custom-enabled-themes (quote (wombat)))
  '(js-indent-level 3)
  '(package-selected-packages (quote (json-mode adoc-mode terraform-mode markdown-mode))))

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

;; Enable line numbers in 
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Enable elpy for Python editing
(elpy-enable)
