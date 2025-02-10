;;; init-text-goodies.el -*- lexical-binding: t; -*-

(use-package rotate-text
  :ensure (:host github :repo "debug-ito/rotate-text.el")
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "]r") #'rotate-text
    (kbd "[r") #'rotate-text-backward))


(use-package google-translate
  :ensure t
  :after evil
  :bind
  (("<leader> lp" . google-translate-at-point)
   ("<leader> lP" . google-translate-at-point-reverse))
  :config
  (setq google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (setq google-translate-default-source-language "it")
  (setq google-translate-default-target-language "en"))

(provide 'init-text-goodies)
