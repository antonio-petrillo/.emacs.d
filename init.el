;;; init.el -*- lexical-binding: t; -*-
;;; Commentary: Emacs init file

(add-to-list 'load-path "lisp")
(add-to-list 'load-path "lisp/programming/")
(add-to-list 'load-path "lisp/experiments/")

(require 'init-elpaca-bootstrap)
(require 'init-emacs)
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

(require 'emacs-lisp-setup)
(require 'go-setup)
(require 'odin-setup)
(require 'zig-setup)
(require 'ocaml-setup)
