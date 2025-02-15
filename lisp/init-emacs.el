;;; init-emacs.el -*- lexical-binding: t; -*-

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
  :custom
  (strokes-file (expand-file-name "strokes" nto-cache))
  (hl-line-sticky-flag nil)
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-autoselect-window nil)
  (strokes-use-strokes-buffer nil)
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

  (global-set-key (kbd "M-<down-mouse-1>") 'strokes-do-stroke)

  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)

  (global-hl-line-mode)
  (auto-save-mode -1)
  (load-theme 'modus-vivendi))

(use-package no-littering
  :ensure t)

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init . delete-selection-mode))

(use-package electric
  :ensure nil
  :hook
  (prog-mode . electric-pair-mode)
  :config
  (setq electric-pair-pairs '((?\{ . ?\})
			      (?\[ . ?\])
			      (?\( . ?\))
			      (?\" . ?\"))))

(use-package tab-bar
  :bind
  (("<leader> <tab>s" . tab-switch)
   ("<leader> <tab><tab>" . tab-switch)
   ("<leader> <tab>c" . tab-close)
   ("<leader> <tab>n" . tab-new)
   ("<leader> <tab>r" . tab-rename)
   ("<leader> <tab>b" . switch-to-buffer-other-tab)
   ("<leader> <tab>d" . dired-other-tab)))

(provide 'init-emacs)
;;; lisp/init-emacs.el ends here
