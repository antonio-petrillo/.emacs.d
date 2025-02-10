;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Emacs init file

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" nto-cache))
;; (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                       :ref nil :depth 1 :inherit ignore
                       :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                       :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
        (elpaca-use-package-mode))

(defun nto/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

;; kill word hack
(defun nto/backward-kill-word()
  "Same as `backward-kill-word' but if it is invoked on a white space character
at the beginning of the line it will stop at it, furthermore if it is invoked
on the beginning of the line it will go the end of the previous line instead
of delete the previous word."
  (interactive)
  (let ((same? (save-excursion
                 (let ((orig (line-number-at-pos (point)))
                       (dest (progn
                               (backward-word)
                               (line-number-at-pos (point)))))
                   (eq orig dest))))
        (start? (eq (point) (line-beginning-position))))
    (cond (start? (backward-delete-char 1))
          (same? (backward-kill-word 1))
          (:else (kill-line 0)))))

(use-package emacs
  :ensure nil
  :init

  (add-hook 'prog-mode-hook (lambda () 
			      (display-line-numbers-mode 1)
			      (setq display-line-numbers 'relative)))

  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (define-key global-map (kbd "M-c") nil)
  (define-key global-map (kbd "C-<backspace>") #'nto/backward-kill-word)
  (define-key global-map (kbd "C-g") #'nto/keyboard-quit-dwim)
  (define-key global-map (kbd "<esc>") #'nto/keyboard-quit-dwim)
  (define-key global-map (kbd "<escape>") #'nto/keyboard-quit-dwim)
  (load-theme 'modus-vivendi))

(use-package hl-line
  :ensure nil
  :config
  (setq hl-line-sticky-flag nil)
  :init
  (global-hl-line-mode))

(use-package which-key
  :ensure t
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  :init
  (which-key-mode))

(use-package no-littering
  :ensure t)

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))

(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-history-directory-alist `(("." . ,(concat nto-cache "undo-tree-hist/"))))
  :init
  (global-undo-tree-mode))

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 100)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

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

(use-package vertico
  :ensure t
  :hook (elpaca-after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init . marginalia-mode))

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

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

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
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-q") nil)

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
  (evil-set-leader 'insert (kbd "<leader> m") t))

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
   ("<leader> fh" . consult-mode-command)
   ("<leader> ff" . consult-find)
   ("<leader> fl" . consult-locate)

   ("<leader> ce" . consult-compile-error)

   ("<leader> bc" . consult-bookmark)

   ("<leader> bb" . consult-buffer)
   ("<leader> bB" . consult-buffer-other-window)

   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)))

(with-eval-after-load 'evil
  (evil-define-key 'normal dired-mode-map
    (kbd "h") #'dired-up-directory
    (kbd "l") #'dired-find-file)

  (evil-define-key nil 'global
    (kbd "<leader> hk") #'describe-key
    (kbd "<leader> hv") #'describe-variable
    (kbd "<leader> hf") #'describe-function
    (kbd "<leader> hc") #'describe-command
    (kbd "<leader> hm") #'describe-map

    (kbd "<leader> .") #'find-file
    (kbd "<leader> fs") #'save-buffer
    (kbd "<leader> fd") #'dired 

    (kbd "<leader> bk") #'kill-this-buffer

    (kbd "<leader> bm") #'bookmark-set
    (kbd "<leader> bd") #'bookmark-delete

    (kbd "<leader> SPC") #'execute-extended-command

    (kbd "<leader> ws") #'evil-window-split
    (kbd "<leader> wv") #'evil-window-vsplit
    (kbd "<leader> wc") #'evil-window-delete
    (kbd "<leader> wh") #'evil-window-left
    (kbd "<leader> wj") #'evil-window-down
    (kbd "<leader> wk") #'evil-window-up
    (kbd "<leader> wl") #'evil-window-right
    (kbd "<leader> w1") #'delete-other-windows
    (kbd "<leader> w0") #'delete-window
    (kbd "<leader> wo") #'other-window))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

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
  ;; evil-multiedit
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

(use-package rotate-text
  :ensure (:host github :repo "debug-ito/rotate-text.el")
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "]r") #'rotate-text
    (kbd "[r") #'rotate-text-backward))

(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-pairs '((?\{ . ?\})
			      (?\[ . ?\])
			      (?\( . ?\))
			      (?\" . ?\")
			      (?\' . ?\')
			      (?\' . ?\'))))

(use-package ace-window
  :ensure t
  :after evil
  :bind
  (("<leader> ww" . ace-window))
  :config
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?c aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'."))

(use-package avy
  :ensure t
  :after evil
  :bind
  (("<leader> jj" . avy-goto-char-timer)
   ("<leader> jl" . avy-goto-line)
   ("<leader> je" . avy-goto-end-of-line)
   ("<leader> jw" . avy-goto-word-0)))

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

(use-package tab-bar
  :after evil
  :bind
  (("<leader> <tab>s" . tab-switch)
   ("<leader> <tab><tab>" . tab-switch)
   ("<leader> <tab>c" . tab-close)
   ("<leader> <tab>n" . tab-new)
   ("<leader> <tab>r" . tab-rename)
   ("<leader> <tab>b" . switch-to-buffer-other-tab)
   ("<leader> <tab>d" . dired-other-tab)))

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

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

(use-package transient
  :ensure t
  :defer t
  :config
  (setq transient-show-popup 0.2))

(use-package magit
  :ensure t
  :bind
  (("<leader> gg" . magit-status)))

(use-package org
  :ensure nil
  :init
  (setq ord-directory (expand-file-name "~/Documents/Org"))
  :bind
  (("<localleader> t" . org-insert-structure-template)
   :map org-mode-map
   ("C-'" . nil)
   ("C-," . nil)
   ("M-;" . nil)
   ("C-c ;" . nil))
  :config
  (setq org-ellipsis "⮧")
  (setq org-adapt-indentation nil)
  (setq org-special-ctrl-a/e nil)
  (setq org-special-ctrl-k nil)
  (setq org-confirm-babel-evaluate nil)
  (setq org-src-window-setup 'current-window)
  (setq org-edit-src-persistent-message nil)
  (setq org-src-fontify-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 0)
  (setq org-export-with-toc t)
  (setq org-export-headline-levels 8)
  (setq org-structure-template-alist
	'(("e" . "src emacs-lisp")
	  ("x" . "export")
	  ("q" . "quote"))))

(use-package denote
  :ensure t
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :config
  (setq denote-directory "~/Documents/Org/notes")
  (setq denote-known-keywords '("emacs" "programming" "algorithm" "datastructure"
                                "pattern" "math" "art" "music"
                                "film" "book" "philosophy" "meta"
                                "linux" "windows" "fitness"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  :bind
  (("<leader> nn" . denote)
   ("<leader> nN" . denote-type)
   ("<leader> nr" . denote-rename-file)
   ("<leader> nR" . denote-rename-file-using-front-matter)
   ("<leader> ni" . denote-link)
   ("<leader> nI" . denote-add-links)
   ("<leader> nb" . denote-backlinks)))

(use-package consult-denote
  :ensure t
  :bind
  (("<leader> nf" . consult-denote-find)
   ("<leader> ng" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(provide 'init)
;;; init.el ends here
