;;; clojure-setup.el -*- lexical-binding: t; -*-

;; DEPS:
;; - clj-kondo
;; - cljfmt
;; - neil

(use-package paredit
  :ensure t)

(use-package parseclj
  :ensure t)

(use-package parseedn
  :ensure t)

(use-package clojure-ts-mode
  :ensure t
  :hook
  ((clojure-mode . subword-mode)
   (clojure-mode . eldoc-mode)
   (clojure-mode . cider-mode))
  :init
  (evil-define-key nil clojure-mode-map
    (kbd "<localleader> '") #'cider-jack-in-clj
    (kbd "<localleader> \"") #'cider-jack-in-cljs
    (kbd "<localleader> m") #'cider-macroexpand-1
    (kbd "<localleader> M") #'cider-macroexpand-all
    (kbd "<localleader> d") #'cider-debug-defun-at-point
    (kbd "<localleader> eb") #'cider-eval-buffer
    (kbd "<localleader> ed") #'cider-eval-defun-at-point
    (kbd "<localleader> ee") #'cider-eval-last-sexp
    (kbd "<localleader> er") #'cider-eval-region
    (kbd "<localleader> hj") #'cider-javadoc
    (kbd "<localleader> hc") #'cider-clojuredocs
    (kbd "<localleader> hn") #'cider-find-ns
    (kbd "<localleader> ha") #'cider-apropos
    (kbd "<localleader> hd") #'cider-doc
    (kbd "<localleader> hw") #'cider-clojuredocs-web
    (kbd "<localleader> nn") #'cider-browse-ns
    (kbd "<localleader> nN") #'cider-browse-ns-all
    (kbd "<localleader> nr") #'cider-ns-refresh
    (kbd "<localleader> nR") #'cider-ns-reload
    (kbd "<localleader> pp") #'cider-pprint-eval-last-sexp
    (kbd "<localleader> pP") #'cider-pprint-eval-last-sexp-to-comment
    (kbd "<localleader> q")  #'cider-quit
    (kbd "<localleader> r")  #'cider-restart
    (kbd "<localleader> tp") #'cider-test-run-project-tests
    (kbd "<localleader> tr") #'cider-test-rerun-failed-tests
    (kbd "<localleader> tt") #'cider-test-run-test))

(use-package clj-refactor
  :ensure t
  :after clojure
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (evil-define-key nil clojure-mode-map
    (kbd "<localleader> r") #'hydra-cljr-help-menu/body))

(use-package cider
  :ensure t
  :after clojure
  :config 
  (setq nrepl-hide-special-buffers t
        nrepl-log-messages nil
        cider-repl-display-help-banner nil
        cider-font-lock-dynamically '(macro core function var deprecated)
        cider-overlays-use-font-lock t
        cider-print-options '(("length" 100))
        cider-prompt-for-symbol nil
        cider-repl-history-display-duplicates nil
        cider-repl-history-display-style 'one-line
        cider-repl-history-file (concat doom-cache-dir "cider-repl-history")
        cider-repl-history-highlight-current-entry t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-size 1000
        cider-repl-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect 'display-only))

(use-package neil
  :commands (neil-find-clojure-package)
  :config
  (setq neil-prompt-for-version-p nil
        neil-inject-dep-to-project-p t)
  (evil-define-key nil clojure-mode-map 
        (kbd "<localleader> f")  #'neil-find-clojure-package))

(provide 'clojure-setup)
