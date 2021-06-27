(load-after-system :nx-search-engines (nyxt-init-file "nx-search-engines/search-engines.lisp"))

;; Define buffer search-engines slot to be a list of several
;; nx-search-engines-provided ones.
(define-configuration (buffer web-buffer)
  ((override-map (keymap:define-key %slot-default%
                   ;; Bind your favorite key to `search-hint'.
                   ;; You need to enable `search-engines-mode' for that.
                   "M-s" 'set-url))
   (search-engines (list (engines:google :shortcut "gmaps"
                                         :object :maps)
                         (engines:wordnet :shortcut "wn"
                                          :show-word-frequencies t)
                         (engines:google :shortcut "goog"
                                         :safe-search nil)))))
