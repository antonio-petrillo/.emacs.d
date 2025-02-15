;;; init-dired.el -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :commands (dired)
  :custom 
  (dired-listing-switches "-aghl -v --group-directories-first")
  :hook
  ((dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-mouse-drag-files t)
  (setq dired-make-directory-clickable t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("TAB" . dired-subtree-toggle)
	("SPC" . nil)
        ("<backtab>" . dired-subtree-remove)
        ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package dired-hide-dotfiles;
  :ensure t
  :after (dired evil-collection)
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map
        ("C-h" . dired-hide-dotfiles-mode)))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(provide 'init-dired)
;;; init-dired.el ends here
