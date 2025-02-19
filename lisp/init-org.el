;;; init-org.el -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :hook
  (org-mode . variable-pitch-mode)
  :bind
  (("C-c a" . org-agenda)
   ("<leader> oa" . org-agenda)
   :map org-mode-map
	("C-'" . nil)
	("C-," . nil)
	("M-;" . nil)
	("M-l" . nil)
	("C-c ;" . nil)
	("<localleader> c" . org-toggle-checkbox)
	("<localleader> di" . org-time-stamp)
	("<localleader> ds" . org-schedule)
	("<localleader> dd" . org-deadline)
	("<localleader> t" . org-todo)
	("<localleader> f" . org-footnote-new))
  :init 
  (setq org-directory (expand-file-name "~/Documents/Org"))
  (setq org-agenda-files `(,(expand-file-name "Inbox.org" org-directory)
                           ,(expand-file-name "Uni.org" org-directory)
                           ,(expand-file-name "Other.org" org-directory)))

  (setq org-agenda-custom-commands
        `(
          ("d" "Daily Agenda"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))))))

  :config
  (add-hook 'org-mode-hook
	    (lambda ()
	      (setq-local electric-pair-inhibit-predicate
			  `(lambda (c)
			     (if (char-equal c ?<) t
			       (,electric-pair-inhibit-predicate c))))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))

  (setq org-agenda-span 'week)
  (setq org-agenda-start-on-weekday 1)
  (setq org-agenda-window-setup 'current-window)

  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
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
  (setq org-cycle-emulate-tab t)
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
