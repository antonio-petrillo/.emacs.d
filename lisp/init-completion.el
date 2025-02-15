;;; init-completion.el -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 5000)
  (setq which-key-idle-secondary-delay 0.05)
  :init
  (which-key-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-count 12)
  :hook (elpaca-after-init . vertico-mode))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :hook (vertico-mode . vertico-mouse-mode))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode)
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle)))

(defun nto/match-components-literally ()
  "Components match literally for the rest of the session."
  (interactive)
  (setq-local orderless-matching-styles '(orderless-literal)
              orderless-style-dispatchers nil))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrride nil))

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode))

(use-package corfu
  :ensure t
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  (corfu-auto t)
  :bind
  (:map corfu-map
        ("<tab>" . corfu-complete)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("C-M-SPC" . corfu-insert-separator)
        ("C-q" . corfu-quick-complete))
  :config
  (setq corfu-cycle t)
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'
  (corfu-history-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :ensure t
  :bind
  (("C-c p" . cape-prefix-map)
   ("M-c f" . cape-file)
   ("M-c D" . cape-dabbrev)
   ("M-c d" . cape-dict)
   ("M-c h" . cape-history)
   ("M-c k" . cape-keyword)
   ("M-c l" . cape-line))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package consult
  :ensure t
  :after evil
  :bind
  (([remap Info-search] . consult-info)
   ("M-y" . consult-yank-pop)
   ("<leader> ht" . consult-theme)

   ("<leader> jc" . consult-line)
   ("<leader> jC" . consult-goto-line)

   ("<leader> fg" . consult-ripgrep)
   ("<leader> /" . consult-ripgrep)
   ("<leader> fh" . consult-mode-command)
   ("<leader> ff" . consult-find)
   ("<leader> fl" . consult-locate)

   ("<leader> ce" . consult-compile-error)

   ("<leader> bc" . consult-bookmark)

   ("<leader> bb" . consult-buffer)
   ("<leader> bB" . consult-buffer-other-window)
   ("<leader> b C-B" . consult-buffer-other-tab)
   ("<leader> b M-B" . consult-buffer-other-frame)

   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("<leader> hB" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
;;; init-completion.el ends here
