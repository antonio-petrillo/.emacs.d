(defvar nto-cache (file-name-concat (getenv "HOME") ".config/nto.d"))
(startup-redirect-eln-cache (expand-file-name "eln" nto-cache))
(setq package-user-dir (expand-file-name "elpa" nto-cache))

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (allow-no-window . t)))

(setq no-littering-etc-directory (expand-file-name "etc" nto-cache))
(setq no-littering-var-directory (expand-file-name "var" nto-cache))

(setq custom-file (file-name-concat nto-cache "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(setq make-backup-files nil)

(provide 'early-init)
;;; early-init.el ends here
