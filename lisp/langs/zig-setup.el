;;; zig-setup.el -*- lexical-binding: t; -*-

(use-package zig-mode
  :ensure t
  :hook (zig-mode . eglot-ensure)
  :config
  (setq zig-format-on-save t)
  (evil-define-key nil zig-mode-map
    (kbd "<localleader> b") #'zig-combile
    (kbd "<localleader> f") #'zig-format-buffer
    (kbd "<localleader> r") #'zig-run
    (kbd "<localleader> t") #'zig-test-buffer))

(provide 'zig-setup)
;;; zig-setup.el ends here
