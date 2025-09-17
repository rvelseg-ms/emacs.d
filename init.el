;; -*- lexical-binding: t -*-

;; Import sensitive configuration file (keys, etc.)
(setq auth-sources '("~/.emacs.d/.authinfo.gpg"))
(let ((sensible-file (expand-file-name "sensible2.el.gpg" user-emacs-directory)))
  (when (file-exists-p sensible-file)
    (load sensible-file)))

(use-package dired-subtree
  :ensure t
  :defer t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<tab>" . dired-subtree-toggle)
              ("C-<tab>" . dired-subtree-cycle))
  :config
  (setq dired-subtree-use-backgrounds nil))

;;; Basic configuration
(setq inhibit-startup-message t)     ; Disable welcome message
(setq initial-scratch-message ";;")  ; Initial *scratch* buffer message
(menu-bar-mode -1)                   ; Disable menu bar
(tool-bar-mode -1)                   ; Disable tool bar
(scroll-bar-mode -1)                 ; Disable scroll bar

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

(use-package f
  :ensure t)

(use-package wfnames
  :ensure t)

(use-package restart-emacs
  :ensure t)

;;; Recentf - Persistent recent files
(use-package recentf
  :ensure nil  ; Included with Emacs
  :init
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 1000
        recentf-auto-cleanup 'never
        recentf-save-file (concat user-emacs-directory "recentf"))
  :config
  (recentf-mode 1)
  ;; Save the list every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list)
  ;; Save on Emacs exit
  (add-hook 'kill-emacs-hook #'recentf-save-list))

;;; Savehist - Persistent history for kill-ring
(use-package savehist
  :ensure nil ; Included with Emacs
  :init
  (setq savehist-file (concat user-emacs-directory ".savehist")
        savehist-additional-variables '(kill-ring))
  :config
  (savehist-mode 1))

;; Line numbers
(global-display-line-numbers-mode 0)

;; Use y/n instead of yes/no for confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; Backup configuration
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;; Package repositories configuration
(require 'package)

;; Package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize the package system
(package-initialize)

;; Refresh package list if it doesn't exist
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Automatically install missing packages

;;; Theme
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
  :ensure t
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

;;; Global key bindings
;; Functions to change font size globally
(defun global-text-scale-increase ()
  "Increase font size globally."
  (interactive)
  (let ((new-height (+ (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height new-height)))

(defun global-text-scale-decrease ()
  "Decrease font size globally."
  (interactive)
  (let ((new-height (- (face-attribute 'default :height) 10)))
    (when (> new-height 50)  ; Avoid fonts that are too small
      (set-face-attribute 'default nil :height new-height))))

(defun global-text-scale-reset ()
  "Reset font size to default."
  (interactive)
  (set-face-attribute 'default nil :height 100))  ; 100 is the default size

;; Change font size globally
(global-set-key (kbd "C-+") 'global-text-scale-increase)
(global-set-key (kbd "C--") 'global-text-scale-decrease)
(global-set-key (kbd "C-=") 'global-text-scale-increase)  ; Alternative without Shift
(global-set-key (kbd "C-0") 'global-text-scale-reset)     ; Reset to original size

;; Resize windows
;;
;; TODO: This collides with something (disabled) in org mode, fix
;; that. It is probably a good idea to override.
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; Windmove for window navigation
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Toggle split direction
(global-set-key (kbd "C-c |") 'toggle-split-direction)

;;; Version control - Magit
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
  ;; Enable refine hunk by default
  (setq magit-diff-refine-hunk 'all))

(use-package forge
  :after magit)

(use-package code-review
  :after forge
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 80)
  (setq code-review-auth-login-marker 'forge)
  :bind (:map forge-topic-mode-map
              ("C-c r" . code-review-forge-pr-at-point)))

;;; Helm - Completion and interface framework
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
        helm-split-window-inside-p nil  ; Changed to nil to use the whole frame
        helm-move-to-line-cycle-in-source t
        helm-echo-input-in-header-line t
        helm-autoresize-max-height 100  ; Use 100% of the height
        helm-autoresize-min-height 100  ; Minimum also 100%
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
  :ensure t
  :defer t
  :bind (("M-i" . swiper-helm)
         ("C-c s" . swiper-helm)))

;;; Dired - File manager
(use-package dired
  :ensure nil  ; Dired is included with Emacs
  :defer t
  :custom
  (dired-listing-switches "-alh --group-directories-first")
  (dired-hide-details-mode t)
  (dired-open-extensions '(("gif" . "eog")
                         ("jpg"   . "eog")
                         ("png"   . "eog")
                         ("mkv"   . "totem")
                         ("mp4"   . "totem")
                         ("pdf"   . "evince")
                         ("html"  . "firefox")
                         ("sh"    . "bash")
                         ("doc"   . "libreoffice")
                         ("docx"  . "libreoffice")
                         ("xls"   . "libreoffice")
                         ("xlsx"  . "libreoffice")
                         ("ppt"   . "libreoffice")
                         ("pptx"  . "libreoffice")
                         ("odt"   . "libreoffice")
                         ("ods"   . "libreoffice")
                         ("odp"   . "libreoffice")))
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1))))

(global-set-key (kbd "C-c p") 'dired-jump)

(use-package dired-k
  :ensure t)

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode))

(defun dired-open-marked-files ()
  "In Dired, open all marked files in new buffers."
  (interactive)
  (mapc #'find-file (dired-get-marked-files)))

(defun jules-dired-open-in-other-window-no-focus ()
  "Open the file under point in another window, keeping focus in Dired."
  (interactive)
  (let ((current-win (selected-window)))
    (dired-find-file-other-window)
    (select-window current-win)))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
              ("." . dired-omit-mode)
	      ("K" . dired-k)
	      ("p" . dired-up-directory)
	      ("F" . dired-open-marked-files)
	      (" " . jules-dired-open-in-other-window-no-focus)
	      ("i" . dired-subtree-toggle))
  :config
  (setq dired-omit-files "^\\...+$")
  ;(setq dired-omit-files (concat dired-omit-files "\\|^\\..*\\|^_.*")
  )

;;; ChatGPT Shell - OpenAI Integration
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
  :bind
  (:map company-active-map
        ([escape] . company-abort)
        ("<return>" . nil)
        ("RET" . nil)
        ("<tab>" . company-complete-selection)
        ("TAB" . company-complete-selection)))

;; ;;; Company backend for org-roam tags
;; (defun my/org-roam-get-all-tags ()
;;   "Consulta la base de datos de org-roam y devuelve una lista
;; con todas las etiquetas únicas."
;;   (when (and (fboundp 'org-roam-db-available-p) (org-roam-db-available-p))
;;     (mapcar #'car (org-roam-db-query '(:select (distinct tag) :from tags)))))

;; (defun company-org-roam-tags (command &optional arg &rest ignored)
;;   "Backend exclusivo de Company para las etiquetas de fichero en org-roam."
;;   (interactive (list 'interactive))
;;   (case command
;;     (interactive (company-begin-backend 'company-org-roam-tags))

;;     (prefix
;;      ;; Este backend solo se activa si estamos en una línea de #+filetags:
;;      (let ((line-content (thing-at-point 'line)))
;;        (when (string-prefix-p "#+filetags:" line-content)
;;          (let* ((line-up-to-point (buffer-substring-no-properties (line-beginning-position) (point)))
;;                 ;; Buscamos el texto que sigue al último ':'
;;                 (prefix (when (string-match ":\\([^:]*\\)$" line-up-to-point)
;;                           (match-string-no-properties 1 line-up-to-point))))
;;            ;; Devolvemos (prefix . t) para que este backend sea 'exclusivo'.
;;            ;; Esto evita que otros backends (como el que busca palabras
;;            ;; en el buffer) ofrezcan sus sugerencias.
;;            (when prefix (cons prefix t))))))

;;     (candidates
;;      ;; Ofrecemos la lista de etiquetas como candidatas.
;;      (let ((tags (my/org-roam-get-all-tags)))
;;        (when tags
;;          (company-filter-candidates arg tags))))

;;     (meta "etiqueta de org-roam")))


;; (defun my/setup-org-roam-tag-completion ()
;;   "Añade el backend de etiquetas de org-roam a company-mode."
;;   ;; Añadimos nuestro nuevo backend a la lista de backends de company.
;;   ;; Lo ponemos al principio para darle prioridad.
;;   (add-to-list 'company-backends 'company-org-roam-tags))

;; ;; Activamos esta configuración cada vez que se abre un archivo en org-mode.
;; (add-hook 'org-mode-hook #'my/setup-org-roam-tag-completion)

;;; Yasnippet - Snippet expansion
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; Python configuration
(use-package python-mode
  :ensure t
  :hook (python-mode . python-ts-mode)
  :bind (:map python-mode-map
              ("C-c C-f" . treesit-fold-toggle)))

(use-package gitlab-ci-mode
  :ensure t
  :defer t)

(use-package gitlab-ci-mode-flycheck
  :ensure t
  :after gitlab-ci-mode)

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org
  :ensure t
  :config
  (setq org-ellipsis " ▼")
  (setq org-hide-emphasis-markers t)
  (setq org-export-in-background t)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))

;; NOTE: org-roam requires sqlite3 to be installed on your system.
;; You can install it with your system's package manager, e.g.,
;; `sudo apt-get install sqlite3` or `brew install sqlite3`.
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-directory (file-truename "~/personal/roam/org"))
  (setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
  (setq org-roam-file-extensions '("org" "md"))
  :custom
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n a" . org-roam-alias-add)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n r" . org-roam-node-random)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; ;; If you're using a vertical completion framework, you might want a more informative completion interface
  ;; (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol)
  )

;; (add-to-list 'load-path "/home/rvelseg/local/md-roam/")
;; (require 'md-roam)
;; (md-roam-mode 1) ; md-roam-mode must be active before org-roam-db-sync
;; (setq md-roam-file-extension "md") ; default "md". Specify an extension such as "markdown"
;; (org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active

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
(add-hook 'html-mode-hook 'visual-line-mode)

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . visual-line-mode)
  :config
  (setq markdown-enable-wiki-links t))

;; Do not truncate the shell buffer
(add-hook 'comint-mode-hook
          (lambda ()
            (setq comint-buffer-maximum-size 100000) ; or nil for unlimited
            (setq comint-scroll-to-bottom-on-input t)
            (setq comint-scroll-show-maximum-output t)
            (setq comint-input-ignoredups t)
            (setq comint-output-filter-functions
                  (remove 'comint-truncate-buffer comint-output-filter-functions))))

;;; treesit-fold configuration
(use-package treesit-fold
  :ensure t)

;; Enable winner-mode for window configuration history
(winner-mode 1)

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
