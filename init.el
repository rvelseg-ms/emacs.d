;; Custom function to handle opening PDFs with evince when '&' is pressed in Dired
(defun my/dired-open-pdf-or-shell ()
  "If on a PDF file in Dired, open it with evince. Otherwise, run dired-do-shell-command."
  (interactive)
  (let ((file (dired-get-filename)))
    (if (and (derived-mode-p 'dired-mode)
             (string-equal (file-name-extension file) "pdf")
             (file-exists-p (expand-file-name file (dired-current-directory))))
        (progn
          (message "Opening PDF with Evince: %s" file)
          ;; Construct the command with full path to evince and quoted filename
          (shell-command (concat "/usr/bin/evince " (shell-quote-argument file))))
      ;; Fallback to the default action for '&', which is dired-do-shell-command
      (dired-do-shell-command))))

;; Bind '&' key in dired-mode-map to the custom function
(eval-after-load 'dired '(progn
  (define-key dired-mode-map (kbd "&") 'my/dired-open-pdf-or-shell)))
