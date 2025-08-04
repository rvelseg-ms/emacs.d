;; -*- lexical-binding: t -*-

;; Importar archivo de configuración sensible (claves, etc.)
(let ((sensible-file (expand-file-name "sensible.el" user-emacs-directory)))
  (when (file-exists-p sensible-file)
    (load sensible-file)))

(use-package dired-subtree
  :defer t
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

(use-package ultra-scroll
  :ensure t
  :init
  ;; Default configuration
  (setq scroll-conservatively 3
        scroll-margin 0)
  :config
  (ultra-scroll-mode 1)
  ;; CUSTOMIZATION:
  ;; To reduce garbage collection pauses during scrolling
  ;; (setq ultra-scroll-gc-percentage 0.8)
  ;; (setq ultra-scroll-gc-idle-time 0.2)

  ;; To change cursor hiding behavior
  ;; (setq ultra-scroll-hide-cursor nil) ; to disable
  ;; (setq ultra-scroll-hide-cursor-delay 0.2) ; delay before hiding

  ;; For mac users, to adjust scroll speed
  ;; (setq ultra-scroll-mac-multiplier 0.9)
  )

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
(global-display-line-numbers-mode 0)

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
;; (use-package zenburn-theme
;;   :config
;;   (load-theme 'zenburn t))

;; TODO: delete-trailing-whitespace for the following modes:
;; - python
;; - org
;; - sh
;; - emacs lisp

;; TODO: enable some module for smooth scrolling.

;; TODO: fix quotes for spanish in org export.

;; TODO: include custom aspell dicts.

;; TODO: Enable flyspell mode.

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(defun toggle-split-direction ()
  "Toggle the split direction of the current window."
  (interactive)
  (if (window-split-horizontally-p)
      (progn
        (delete-other-windows)
        (split-window-vertically))
    (progn
      (delete-other-windows)
      (split-window-horizontally))))

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
;;
;; TODO: This collides with something (disabled) in org mode, fix
;; that. It is probably a good idea to override.
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; Windmove para navegación de ventanas
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Toggle split direction
(global-set-key (kbd "C-c |") 'toggle-split-direction)

;;; Control de versiones - Magit
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x v d" . magit-status)
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
  :defer t
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
        helm-display-buffer-default-height 100
        helm-mini-default-sources '(helm-source-buffers-list
                                     helm-source-recentf
                                     helm-source-bookmarks))
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-for-files)
         ("C-x C-b" . helm-for-files)
         ("C-x C-r" . helm-recentf)
         ("C-c h o" . helm-occur)
         ("C-c h g" . helm-google-suggest)))

(use-package swiper-helm
  :defer t
  :bind (("M-i" . swiper-helm)
         ("C-c s" . swiper-helm)))

;;; Dired - Administrador de archivos
(use-package dired
  :ensure nil  ; Dired viene incluido con Emacs
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-hide-details-mode t)
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))))

(global-set-key (kbd "C-c p") 'dired-jump)

(use-package dired-k)

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("." . dired-omit-mode)
	      ("K" . dired-k)
	      ("i" . dired-subtree-toggle))
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..*\\|^_.*"))
  )

;;; ChatGPT Shell - Integración con OpenAI
(use-package chatgpt-shell
  :ensure t
  :defer t
  :bind (("C-c c c" . chatgpt-shell)
         ("C-c c r" . chatgpt-shell-send-region)
         ("C-c c b" . chatgpt-shell-send-buffer)
         ("C-c c p" . chatgpt-shell-prompt-compose)
         ("C-c c s" . chatgpt-shell-send-and-review-region))
  :config
  (setq chatgpt-shell-openai-key openai-key))

;;; Org Mode
;;; Company Mode
(use-package company
  :ensure t
  :defer t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org
  :ensure t
  :config
  (setq org-ellipsis " ▼"))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/roam"))
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (setq org-roam-file-extensions '("org" "md"))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol)
  )

(add-to-list 'load-path "/home/rvelseg/local/md-roam/")
(require 'md-roam)
(md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
(setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;;; Visual line mode for specific modes
(add-hook 'magit-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'html-mode-hook 'visual-line-mode)

(setq markdown-enable-wiki-links t)

;; No truncar el buffer del shell
(add-hook 'comint-mode-hook
          (lambda ()
            (setq comint-buffer-maximum-size 100000) ; o nil para ilimitado
            (setq comint-scroll-to-bottom-on-input t)
            (setq comint-scroll-show-maximum-output t)
            (setq comint-input-ignoredups t)
            (setq comint-output-filter-functions
                  (remove 'comint-truncate-buffer comint-output-filter-functions))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(f sqlite3 tabbar session pod-mode muttrc-mode mutt-alias markdown-mode magit initsplit htmlize helm-org graphviz-dot-mode folding eproject diminish csv-mode company color-theme-modern browse-kill-ring boxquote bm bar-cursor apache-mode zenburn-theme wfnames use-package swiper-helm restart-emacs org-roam-ui org-bullets dired-subtree dired-hide-dotfiles chatgpt-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

