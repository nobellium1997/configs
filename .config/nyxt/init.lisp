(load-after-system :nx-search-engines (nyxt-init-file "nx-search-engines/search-engines.lisp"))

;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
(define-configuration (buffer web-buffer)
   ((override-map (let ((map (make-keymap "override-map")))
                             (define-key map
                               "M-s" 'set-url
                               "M-S" 'set-url-new-buffer
                               "M-r" 'reload-current-buffer)
                   map))
   (search-engines (list (engines:google :shortcut "gmaps"
                                         :object :maps)
                         (engines:wikipedia :shortcut "wiki")
                         (engines:google :shortcut "goog"
                                         :safe-search nil)))))
