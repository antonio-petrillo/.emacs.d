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
(setq use-short-answers t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

(add-to-list 'default-frame-alist
             '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist
             '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist
             '(vertical-scroll-bars . nil))

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar nto/file-name-handler-alist file-name-handler-alist)
(defvar nto/vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist nto/file-name-handler-alist
                  vc-handled-backends nto/vc-handled-backends)))

(setq-default truncate-lines t)
