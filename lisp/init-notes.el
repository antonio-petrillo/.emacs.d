;;; init-notes.el -*- lexical-binding: t; -*-

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

(provide 'init-notes)
