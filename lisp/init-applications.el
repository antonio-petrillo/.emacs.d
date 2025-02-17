;;; init-applications.el -*- lexical-binding: t; -*-

(use-package xdg-appmenu
  :ensure (:host codeberg :repo "akib/emacs-xdg-appmenu")
  :bind
  (("C-<f2>" . xdg-appmenu)))

(provide 'init-applications)
;;; init-applications.el ends here
