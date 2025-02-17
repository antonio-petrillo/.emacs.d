;;; init-eshell.el -*- lexical-binding: t; -*-
(use-package eshell
  :ensure nil
  :config 
  (setq eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
  (setq eshell-aliases-file (expand-file-name "aliases" eshell-directory-name))
  :bind
  (("<leader> oe" . eshell)
   :map eshell-mode-map
   ("C-p" . eshell-previous-input)
   ("C-n" . eshell-next-input)
   ("C-l" . eshell/clear)))

(provide 'init-eshell)
;;; init-eshell.el ends here
