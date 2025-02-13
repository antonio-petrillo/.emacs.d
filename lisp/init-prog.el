;;; init-prog.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp/langs/" user-emacs-directory))

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
