;;; init-proj.el -*- lexical-binding: t; -*-

(use-package project
  :ensure nil
  :bind
  (("<leader> pp" . project-switch-project)
   ("<leader> pb" . project-switch-buffer)
   ("<leader> pc" . project-compile)
   ("<leader> ps" . project-shell)
   ("<leader> pe" . project-eshell)
   ("<leader> pf" . project-find-file)
   ("<leader> pk" . project-kill-buffers)
   ("<leader> p&" . project-async-shell-command)))

(use-package transient
  :ensure t
  :defer t
  :config
  (setq transient-show-popup 0.2))

(use-package magit
  :ensure t
  :bind
  (("<leader> gg" . magit-status)))

(provide 'init-proj)
