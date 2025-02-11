;;; odin-setup.el -*- lexical-binding: t; -*-

(use-package odin-mode
  :ensure (:host sourcehut :repo "mgmarlow/odin-mode")
  :config
  :config
  (evil-define-key nil go-mode-map 
    (kbd "<localleader> b")  #'odin-build-project
    (kbd "<localleader> c")  #'odin-check-project
    (kbd "<localleader> r")  #'odin-run-project
    (kbd "<localleader> t")  #'odin-test-project))

(provide 'odin-setup)
