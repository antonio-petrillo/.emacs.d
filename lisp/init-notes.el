;;; init-notes.el -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (markdown-mode . denote-dired-mode))
  :config
  (setq denote-directory "~/Documents/Org/notes")
  (setq denote-known-keywords '("emacs" "programming" "algorithm"
				"datastructure" "cryptography" "logbook"
                                "film" "book" "meta"
                                "linux" "windows" "fitness"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  :bind
  (("<leader> nn" . denote)
   ("<leader> nf" . denote-open-or-create)
   ("<leader> nN" . denote-type)
   ("<leader> nr" . denote-rename-file)
   ("<leader> nR" . denote-rename-file-using-front-matter)
   ("<leader> ni" . denote-link)
   ("<leader> nh" . denote-org-extras-link-to-heading)
   ("<leader> nI" . denote-add-links)
   ("<leader> nb" . denote-backlinks)
   ("<leader> nB" . denote-org-extras-backlinks-for-heading)

   ("<leader> nss" . denote-sequence)
   ("<leader> nsi" . denote-sequence-link)))

(use-package consult-denote
  :ensure t
  :bind
  (("<leader> nF" . consult-denote-find)
   ("<leader> ng" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(provide 'init-notes)
;;; init-notes.el ends here
