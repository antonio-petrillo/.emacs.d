;;; init-prog.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "lisp/langs/")

(use-package eglot
  :ensure nil
  :functions (eglot-ensure)
  :commands (eglot)
  :config
  (setq eglot-autoshutdown t))

(defun nto/enable-languages (languages)
  (dolist (lang languages)
    (require (intern (format "%s-setup" lang)))))

(provide 'init-prog)
