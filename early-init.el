(defvar nto-cache (file-name-concat (getenv "HOME") ".config/nto.d"))
(startup-redirect-eln-cache (expand-file-name "eln" nto-cache))
(setq package-user-dir (expand-file-name "elpa" nto-cache))

(setq no-littering-etc-directory (expand-file-name "etc" nto-cache))
(setq no-littering-var-directory (expand-file-name "var" nto-cache))

(setq custom-file (file-name-concat nto-cache "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(setq make-backup-files nil)

(setq package-enable-at-startup nil)
(setq evil-want-keybinding nil)

(add-to-list 'default-frame-alist
             '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist
             '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(provide 'early-init)
;;; early-init.el ends here
