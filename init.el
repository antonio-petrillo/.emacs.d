;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Emacs init file

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(defun nto/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'nto/display-startup-time)

(require 'init-elpaca-bootstrap)
(require 'init-emacs)
(require 'init-windows)
(require 'init-dired)
(require 'init-completion)
(require 'init-evil)
(require 'init-jumps)
(require 'init-org)
(require 'init-notes)
(require 'init-ui)
(require 'init-proj)
(require 'init-text-goodies)
(require 'init-snippets)
(require 'init-themes)
(require 'init-fonts)
(require 'init-strokes)
(require 'init-eshell)

(require 'init-prog)
(with-eval-after-load 'init-prog
  (nto/enable-languages
   '(emacs-lisp
     clojure
     go
     odin
     zig
     ocaml)))
