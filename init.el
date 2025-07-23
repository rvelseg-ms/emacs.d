;; Importar archivo de configuración sensible (claves, etc.)
(let ((sensible-file (expand-file-name "sensible.el" user-emacs-directory)))
  (when (file-exists-p sensible-file)
    (load sensible-file)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)
              ("C-<tab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;; Configuración básica
(setq inhibit-startup-message t)     ; Desactivar mensaje de bienvenida
(setq initial-scratch-message ";;")  ; Mensaje inicial del buffer *scratch*
(menu-bar-mode -1)                   ; Desactivar barra de menú
(tool-bar-mode -1)                   ; Desactivar barra de herramientas
(scroll-bar-mode -1)                 ; Desactivar barra de desplazamiento

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

;; Usar y/n en lugar de yes/no para confirmaciones
(fset 'yes-or-no-p 'y-or-n-p)

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
;; Funciones para cambiar tamaño de fuente globalmente
(defun global-text-scale-increase ()
  "Aumentar tamaño de fuente globalmente."
  (interactive)
  (let ((new-height (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)))

(defun global-text-scale-decrease ()
  "Disminuir tamaño de fuente globalmente."
  (interactive)
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (when (> new-height 50)  ; Evitar fuentes demasiado pequeñas
      (set-face-attribute 'default nil :height new-height))))

(defun global-text-scale-reset ()
  "Resetear tamaño de fuente al valor por defecto."
  (interactive)
  (set-face-attribute 'default nil :height 100))  ; 100 es el tamaño por defecto

;; Cambiar tamaño de fuente globalmente
(global-set-key (kbd "C-+") 'global-text-scale-increase)
(global-set-key (kbd "C--") 'global-text-scale-decrease)
(global-set-key (kbd "C-=") 'global-text-scale-increase)  ; Alternativo sin Shift
(global-set-key (kbd "C-0") 'global-text-scale-reset)     ; Resetear a tamaño original

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
         ("C-c g d" . magit-diff-buffer-file)
         ("C-c g r" . magit-diff-toggle-refine-hunk))
  :config
  ;; Activar refine hunk por defecto
  (setq magit-diff-refine-hunk 'all))

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
  (dired-hide-details-mode t)  ; Activar por defecto globalmente
  :config
  ;; Asegurar que siempre esté activado
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))))

(use-package dired-hide-dotfiles
  :after dired
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode))
  :config
  ;; Ocultar archivos ocultos por defecto con hook más específico
  (add-hook 'dired-after-readin-hook 'dired-hide-dotfiles-mode))

;;; ChatGPT Shell - Integración con OpenAI
(use-package chatgpt-shell
  :ensure t
  :bind (("C-c c c" . chatgpt-shell)
         ("C-c c r" . chatgpt-shell-send-region)
         ("C-c c b" . chatgpt-shell-send-buffer)
         ("C-c c p" . chatgpt-shell-prompt-compose)
         ("C-c c s" . chatgpt-shell-send-and-review-region))
  :config
  (setq chatgpt-shell-openai-key openai-key))

;;; Visual line mode for specific modes
(add-hook 'magit-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'html-mode-hook 'visual-line-mode)
