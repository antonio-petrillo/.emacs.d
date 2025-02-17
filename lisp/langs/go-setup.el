;;; go-setup.el -*- lexical-binding: t; -*-

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure)
  :bind
  (("<localleader> f" . gofmt)))

(use-package go-eldoc
  :ensure t
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'go-setup)
;;; go-setup.el ends here
