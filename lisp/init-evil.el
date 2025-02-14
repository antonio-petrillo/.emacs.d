(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree-hist/" nto-cache))))
  :init
  (global-undo-tree-mode))

(use-package evil
  :ensure t
  :after undo-tree
  :hook (elpaca-after-init . evil-mode)
  :custom
  (evil-undo-system 'undo-tree)
  :config
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-delete nil)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-d-scroll nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)

  (evil-set-initial-state 'calc-mode 'emacs)

  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-q") nil)
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)

  (evil-set-leader 'normal (kbd "SPC") nil)
  (evil-set-leader 'visual (kbd "SPC") nil)
  (evil-set-leader 'operator (kbd "SPC") nil)
  (evil-set-leader 'replace (kbd "SPC") nil)
  (evil-set-leader 'motion (kbd "SPC") nil)
  (evil-set-leader 'insert (kbd "C-SPC") nil)

  (evil-set-leader 'normal (kbd "<leader> m") t)
  (evil-set-leader 'visual (kbd "<leader> m") t)
  (evil-set-leader 'operator (kbd "<leader> m") t)
  (evil-set-leader 'replace (kbd "<leader> m") t)
  (evil-set-leader 'motion (kbd "<leader> m") t)
  (evil-set-leader 'insert (kbd "<leader> m") t)

  (evil-define-key 'normal dired-mode-map
    (kbd "h") #'dired-up-directory
    (kbd "l") #'dired-find-file)

  (evil-define-key nil 'global
    (kbd "<leader> hk") #'describe-key
    (kbd "<leader> hv") #'describe-variable
    (kbd "<leader> hf") #'describe-function
    (kbd "<leader> hc") #'describe-command
    (kbd "<leader> hm") #'describe-keymap

    (kbd "<leader> .") #'find-file
    (kbd "<leader> fs") #'save-buffer
    (kbd "<leader> fd") #'dired 

    (kbd "<leader> bk") #'kill-this-buffer
    (kbd "<leader> br") #'revert-buffer

    (kbd "<leader> bm") #'bookmark-set
    (kbd "<leader> bd") #'bookmark-delete

    (kbd "<leader> SPC") #'execute-extended-command

    (kbd "<leader> ws") #'evil-window-split
    (kbd "<leader> wv") #'evil-window-vsplit
    (kbd "<leader> wc") #'evil-window-delete
    (kbd "<leader> wh") #'evil-window-left
    (kbd "<leader> wM") #'toggle-frame-maximized
    (kbd "<leader> wj") #'evil-window-down
    (kbd "<leader> wk") #'evil-window-up
    (kbd "<leader> wl") #'evil-window-right
    (kbd "<leader> w1") #'delete-other-windows
    (kbd "<leader> wm") #'delete-other-windows
    (kbd "<leader> w0") #'delete-window
    (kbd "<leader> wo") #'other-window)

  (evil-define-key nil 'global
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line
    (kbd "C-f") #'forward-char
    (kbd "C-b") #'backward-char
    (kbd "C-p") #'previous-line
    (kbd "C-n") #'next-line
    (kbd "C-d") #'delete-char))

(use-package evil-collection
  :ensure t
  :after evil
  :init
  (evil-collection-init))

(with-eval-after-load 'evil-collection
  (setq evil-collection-key-blacklist
        (append '("SPC" "C-SPC" "SPC m" "C-SPC m")
                evil-collection-key-blacklist
		'("gd" "gf" "K")
                '("gr" "gR")
                '("[" "]" "gz"))))

(use-package evil-escape
  :ensure t
  :after evil
  :hook (evil-mode . evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2))

(use-package evil-exchange
  :ensure t
  :after evil
  :commands evil-exchange
  :init
  (evil-exchange-install))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :ensure t
  :after evil
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :hook (evil-mode . evil-snipe-override-mode)
  :hook (evil-mode . evil-snipe-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t))

(use-package evil-visualstar
  :ensure t
  :after evil
  :hook (evil-mode . global-evil-visualstar-mode)
  :config
  (setq-default evil-visualstart/persistent t))

(use-package exato
  :ensure t
  :after evil)

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (evil-define-key '(visual normal) 'global "gc" #'evilnc-comment-operator))

(use-package evil-textobj-anyblock
  :ensure t
  :after evil
  :init
  (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "M-a")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-A")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-a")   #'evil-multiedit-match-and-next
    (kbd "M-A")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-a") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-a")   #'evil-multiedit-match-and-next
      (kbd "M-S-a") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev)))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-traces
  :ensure t
  :config
  (evil-traces-use-diff-faces) 
  (evil-traces-mode))

(provide 'init-evil)
;;; init-evil.el ends here
