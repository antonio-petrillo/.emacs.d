;;; init-org.el -*- lexical-binding: t; -*-

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

(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(provide 'init-org)
