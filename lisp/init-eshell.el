;;; init-eshell.el -*- lexical-binding: t; -*-
(use-package eshell
  :ensure nil
  :config 
  (setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (setq eshell-aliases-file (expand-file-name "aliases" eshell-directory-name))
  :bind
  (("<leader> ot" . eshell)))

(provide 'init-eshell)
;;; init-eshell.el ends here
