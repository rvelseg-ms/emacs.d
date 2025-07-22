;;; Configuración básica
(setq inhibit-startup-message t)  ; Desactivar mensaje de bienvenida
(menu-bar-mode -1)                ; Desactivar barra de menú
(tool-bar-mode -1)                ; Desactivar barra de herramientas
(scroll-bar-mode -1)              ; Desactivar barra de desplazamiento

;; Frame inicial con fondo oscuro
(add-to-list 'default-frame-alist '(background-color . "#3f3f3f"))
(add-to-list 'default-frame-alist '(foreground-color . "#dcdccc"))
(add-to-list 'initial-frame-alist '(background-color . "#3f3f3f"))
(add-to-list 'initial-frame-alist '(foreground-color . "#dcdccc"))

;;; Recentf - Archivos recientes persistentes
(use-package recentf
  :ensure nil  ; Viene incluido con Emacs
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never
        recentf-save-file (concat user-emacs-directory "recentf"))
  :config
  (recentf-mode 1)
  ;; Guardar la lista cada 5 minutos
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; Guardar al salir de Emacs
  (add-hook 'kill-emacs-hook #'recentf-save-list))

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

;;; Key bindings globales
;; Cambiar tamaño de fuente globalmente
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)  ; Alternativo sin Shift
(global-set-key (kbd "C-0") 'text-scale-adjust)    ; Resetear a tamaño original

;; Redimensionar ventanas/windows
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

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
        helm-split-window-inside-p nil  ; Cambiado a nil para usar todo el frame
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 100  ; Usar el 100% de la altura
        helm-autoresize-min-height 100  ; Mínimo también 100%
        helm-display-buffer-default-height 100)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)  ; Cambiado a helm-mini
         ("C-x C-r" . helm-recentf)
         ("C-c h o" . helm-occur)
         ("C-c h g" . helm-google-suggest)))

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

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)
              ("C-<tab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))
