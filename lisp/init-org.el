;;; init-org.el -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook
  (org-mode . variable-pitch-mode)
  :init
  (setq ord-directory (file-name-concat (getenv "HOME") "Documents/Org"))
  :bind
  (:map org-mode-map
	("C-'" . nil)
	("C-," . nil)
	("M-;" . nil)
	("M-l" . nil)
	("C-c ;" . nil))
  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-local electric-pair-inhibit-predicate
			  `(lambda (c)
			     (if (char-equal c ?<) t
			       (,electric-pair-inhibit-predicate c))))))

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
  (setq org-export-headline-levels 8))

(use-package org-modern
  :ensure t
  :after org
  :custom
  (org-modern-table nil)
  :hook ((org-mode . org-modern-mode)
	 (org-agenda-finalize . org-modern-agenda)))

(provide 'init-org)
;;; init-org.el ends here
