;;; init-eat.el -*- lexical-binding: t; -*-
(use-package eat
  :ensure (:host codeberg :repo "akib/emacs-eat")
  :init 
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  :bind
  (("<leader> ot" . eat)))

(provide 'init-eat)
;;; init-eat.el ends here
