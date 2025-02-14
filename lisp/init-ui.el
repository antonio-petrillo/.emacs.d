;;; init-ui.el -*- lexical-binding: t; -*-

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package breadcrumb
  :ensure t
  :bind (("<leader> tb" . breadcrumb-local-mode)))

(use-package rainbow-mode
  :ensure t
  :commands (rainbow-mode)
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-green
          pulsar-highlight-face 'pulsar-magenta)
  (pulsar-global-mode 1))

(use-package doom-modeline
  :ensure t
  :hook (elpaca-after-init . doom-modeline-mode)
  :hook (doom-modeline-mode . size-indication-mode)
  :hook (doom-modeline-mode . column-number-mode)
  :config
  (setq doom-modeline-lsp-icon nil))

(provide 'init-ui)
;;; init-ui.el ends here
