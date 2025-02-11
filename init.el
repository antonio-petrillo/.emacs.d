;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Emacs init file

(add-to-list 'load-path "lisp")
(add-to-list 'load-path "lisp/experiments/")

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

(require 'init-prog)
(with-eval-after-load 'init-prog
  (nto/enable-languages
   '(emacs-lisp
     go
     odin
     zig
     ocaml)))
