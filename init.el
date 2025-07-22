;; init.el - Configuración personal de Emacs
;; Autor: [Tu nombre]
;; Fecha: 22 de Julio, 2025

;;; Configuración básica
(setq inhibit-startup-message t)  ; Desactivar mensaje de bienvenida
(menu-bar-mode -1)                ; Desactivar barra de menú
(tool-bar-mode -1)                ; Desactivar barra de herramientas
(scroll-bar-mode -1)              ; Desactivar barra de desplazamiento

;; Números de línea
(global-display-line-numbers-mode 1)

;; Configuración de respaldo
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; Configuración de repositorios de paquetes
(require 'package)

;; Repositorios de paquetes
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Inicializar el sistema de paquetes
(package-initialize)

;; Actualizar la lista de paquetes si no existe
(unless package-archive-contents
  (package-refresh-contents))

;; Instalar use-package si no está instalado
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Instalar automáticamente paquetes faltantes

;;; Tema
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;; Control de versiones - Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-current)
         ("C-c g f" . magit-log-buffer-file)
         ("C-c g d" . magit-diff-buffer-file)))

;;; Helm - Framework de completado e interfaz
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-locate-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-candidate-number-limit 150
        helm-split-window-inside-p t
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 0
        helm-autoresize-min-height 20)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  ;; Definir el comando prefix primero
  (define-prefix-command 'helm-command-prefix)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x C-r" . helm-recentf)
         :map helm-command-prefix-map
         ("o" . helm-occur)
         ("g" . helm-google-suggest)
         ("M-x" . helm-M-x)))

(use-package swiper-helm
  :bind (("M-i" . swiper-helm)
         ("C-c s" . swiper-helm)))

;;; Dired - Administrador de archivos
(use-package dired
  :ensure nil  ; Dired viene incluido con Emacs
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  :config
  ;; Ocultar detalles por defecto
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;;; Aquí irán tus configuraciones adicionales...
