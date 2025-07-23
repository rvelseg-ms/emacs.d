;; -*- lexical-binding: t -*-

;; Importar archivo de configuración sensible (claves, etc.)
(let ((sensible-file (expand-file-name "sensible.el" user-emacs-directory)))
  (when (file-exists-p sensible-file)
    (load sensible-file)))

;;; ---------------------------------------------------------------------------
;;; Configuración de repositorios de paquetes
;;; ---------------------------------------------------------------------------
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; ---------------------------------------------------------------------------
;;; use-package: Instalación y configuración
;;; ---------------------------------------------------------------------------
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; ---------------------------------------------------------------------------
;;; Configuración básica de la UI
;;; ---------------------------------------------------------------------------
(setq inhibit-startup-message t)
(setq initial-scratch-message ";;")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)

;; Frame inicial con fondo oscuro
(add-to-list 'default-frame-alist '(background-color . "#3f3f3f"))
(add-to-list 'default-frame-alist '(foreground-color . "#dcdccc"))
(add-to-list 'initial-frame-alist '(background-color . "#3f3f3f"))
(add-to-list 'initial-frame-alist '(foreground-color . "#dcdccc"))

;; Usar y/n en lugar de yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;;; ---------------------------------------------------------------------------
;;; Configuración de respaldo
;;; ---------------------------------------------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; ---------------------------------------------------------------------------
;;; Tema
;;; ---------------------------------------------------------------------------
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;;; ---------------------------------------------------------------------------
;;; Key bindings globales
;;; ---------------------------------------------------------------------------

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
    (when (> new-height 50)
      (set-face-attribute 'default nil :height new-height))))

(defun global-text-scale-reset ()
  "Resetear tamaño de fuente al valor por defecto."
  (interactive)
  (set-face-attribute 'default nil :height 100))

(global-set-key (kbd "C-+") 'global-text-scale-increase)
(global-set-key (kbd "C--") 'global-text-scale-decrease)
(global-set-key (kbd "C-=") 'global-text-scale-increase)
(global-set-key (kbd "C-0") 'global-text-scale-reset)

;; Redimensionar ventanas/windows
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;;; ---------------------------------------------------------------------------
;;; recentf: Archivos recientes
;;; ---------------------------------------------------------------------------
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never
        recentf-save-file (concat user-emacs-directory "recentf"))
  :config
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (add-hook 'kill-emacs-hook #'recentf-save-list))

;;; ---------------------------------------------------------------------------
;;; Magit: Control de versiones
;;; ---------------------------------------------------------------------------
(use-package magit
  :defer t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c g b" . magit-blame)
         ("C-c g l" . magit-log-current)
         ("C-c g f" . magit-log-buffer-file)
         ("C-c g d" . magit-diff-buffer-file)
         ("C-c g r" . magit-diff-toggle-refine-hunk))
  :config
  (setq magit-diff-refine-hunk 'all))

;;; ---------------------------------------------------------------------------
;;; Helm: Framework de completado
;;; ---------------------------------------------------------------------------
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
        helm-split-window-inside-p nil
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 100
        helm-autoresize-min-height 100
        helm-display-buffer-default-height 100)
  :config
  (helm-mode 1)
  (helm-autoresize-mode 1)
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-x C-r" . helm-recentf)
         ("C-c h o" . helm-occur)
         ("C-c h g" . helm-google-suggest)))

(use-package swiper-helm
  :defer t
  :bind (("M-i" . swiper-helm)
         ("C-c s" . swiper-helm)))

;;; ---------------------------------------------------------------------------
;;; Dired: Administrador de archivos
;;; ---------------------------------------------------------------------------
(use-package dired
  :ensure nil
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-hide-details-mode t)
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))))

(use-package dired-hide-dotfiles
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("." . dired-hide-dotfiles-mode))
  :config
  (add-hook 'dired-after-readin-hook 'dired-hide-dotfiles-mode))

(use-package dired-subtree
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)
              ("C-<tab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;; ---------------------------------------------------------------------------
;;; ChatGPT Shell: Integración con OpenAI
;;; ---------------------------------------------------------------------------
(use-package chatgpt-shell
  :defer t
  :bind (("C-c c c" . chatgpt-shell)
         ("C-c c r" . chatgpt-shell-send-region)
         ("C-c c b" . chatgpt-shell-send-buffer)
         ("C-c c p" . chatgpt-shell-prompt-compose)
         ("C-c c s" . chatgpt-shell-send-and-review-region))
  :config
  (setq chatgpt-shell-openai-key openai-key))
