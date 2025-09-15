;; init.el

(message "+++ init.el load +++")

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Disable the 'Creator' meta tag by default
(setq org-export-with-creator nil)

;; install org if missing…
(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))
