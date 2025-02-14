;;; odin-setup.el -*- lexical-binding: t; -*-
(use-package odin-mode
  :ensure (:host sourcehut :repo "mgmarlow/odin-mode")
  :hook (odin-mode . eglot-ensure)
  :config
  (evil-define-key nil go-mode-map 
    (kbd "<localleader> b")  #'odin-build-project
    (kbd "<localleader> c")  #'odin-check-project
    (kbd "<localleader> r")  #'odin-run-project
    (kbd "<localleader> t")  #'odin-test-project))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(odin-mode . ("ols"))))


(provide 'odin-setup)
;;; odin-setup.el ends here
