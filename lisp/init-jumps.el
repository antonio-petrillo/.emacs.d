;;; init-jumps.el -*- lexical-binding: t; -*-

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

(provide 'init-jumps)
