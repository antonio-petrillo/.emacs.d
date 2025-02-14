;;; init-window.el -*- lexical-binding: t; -*-

(setq display-buffer-alist
      `(("\\*Occur\\*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (dedicated . t)
         (window-height . fit-window-to-buffer))

	("\\`\\*Async Shell Command\\*\\'"
	 (display-buffer-no-window))

	("\\`\\*\\(Warnings\\|Compile-Log\\|Org Links\\)\\*\\'"
	 (display-buffer-no-window)
	 (allow-no-window . t))

	((or . ((derived-mode . occur-mode)
		(derived-mode . grep-mode)
		(derived-mode . Buffer-menu-mode)
		(derived-mode . log-view-mode)
		(derived-mode . help-mode)
		"\\*\\(|Buffer List\\|Occur\\|vc-change-log\\|eldoc.*\\).*"))
	 (display-buffer-reuse-window
	  display-buffer-below-selected)
	 (dedicated . t)
	 (window-height . fit-window-to-buffer)
	 (body-function . (lambda (window) (select-window window))))))

(use-package winner
  :ensure nil
  :hook (elpaca-after-init . winner-mode)
  :bind
  (("<leader> wu" . winner-undo)
   ("<leader> wr" . winner-redo)))

(use-package spacious-padding
  :ensure t
  :hook ((org-mode text-mode) . spacious-padding-mode)
  :if (display-graphic-p)
  :bind
  (("<leader> ts" . spacious-padding-mode))
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line-active
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive)))

(provide 'init-windows)
;;; lisp/init-windows.el ends here
