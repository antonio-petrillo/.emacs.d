;;; go-setup.el -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure)
  :config
  (evil-define-key nil go-mode-map 
    (kbd "<localleader> a")  #'go-tag-add
    (kbd "<localleader> d")  #'go-tag-remove
    (kbd "<localleader> i")  #'go-goto-imports))

(provide 'go-setup)
