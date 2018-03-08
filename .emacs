;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Añade los repositorios de paquetes de MELPA.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

;; Pone el modo AsciiDoc al abrir los ficheros .adoc.
(add-to-list
  'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

;; Deshabilita el sonido.
(setq visible-bell t)

;; Utiliza espacios en vez de tabuladores.
(setq-default indent-tabs-mode nil)

;; Fijamos la tabulación por defecto a tres espacios.
(setq-default tab-width 3)

;; Fijamos la tabulación del buffer actual a tres espacios.
(setq tab-width 3)

;; Mostramos el número de columna en la barra de estado.
(setq column-number-mode t)

;; Fijamos las paradas del tabulador cada 3 caracteres.
(setq tab-stop-list (number-sequence 3 200 3))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 3)
 '(package-selected-packages (quote (adoc-mode markdown-mode))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
