;;; init-fonts.el -*- lexical-binding: t; -*-

(let ((mono-spaced-font "Aporetic Serif Mono") ;; "Monospace" backup
      (proportionately-spaced-font "Aporetic Sans")) ;; "Sans" backup

  (set-face-attribute
   'default nil
   :family mono-spaced-font
   :height 120)

  (set-face-attribute
   'fixed-pitch nil
   :family mono-spaced-font
   :height 1.0)

  (set-face-attribute
   'variable-pitch nil
   :family proportionately-spaced-font
   :height 1.0))

(provide 'init-fonts)
;;; init-fonts.el ends here
