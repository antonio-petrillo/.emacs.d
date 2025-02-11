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
  :custom
  (google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (google-translate-default-source-language "it")
  (google-translate-default-target-language "en")
  :bind
  (("<leader> lp" . google-translate-at-point)
   ("<leader> lP" . google-translate-at-point-reverse))
  :init
  (add-to-list 'display-buffer-alist
	       '("\\*Google Translate\\*"
		 (display-buffer-reuse-window
		  display-buffer-below-selected)
		 (dedicated . t)
		 (window-height . fit-window-to-buffer)
		 (body-function . (lambda (window) (select-window window))))))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package drag-stuff
  :ensure t
  :bind
  (("M-j" . drag-stuff-down)
   ("M-k" . drag-stuff-up))
  :init
  (drag-stuff-global-mode 1))

(provide 'init-text-goodies)
