(load-after-system :nx-search-engines (nyxt-init-file "nx-search-engines/search-engines.lisp"))

(define-configuration (buffer web-buffer)
    ((default-modes (append '(blocker-mode vi-normal-mode vi-insert-mode) %slot-default%))
     (search-engines (list (engines:google :shortcut "gmaps"
                                           :object :maps)
                           (engines:wikipedia :shortcut "wiki")
                           (engines:google :shortcut "goog"
                                           :safe-search nil)))))

;; (defun eval-in-emacs (&rest s-exps)
;;   "Evaluate S-EXPS with emacsclient."
;;   (let ((s-exps-string (cl-strings:replace-all
;;                         (write-to-string
;;                          `(progn ,@s-exps) :case :downcase)
;;                         ;; Discard the package prefix.
;;                         "nyxt::" "")))
;;     (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
;;     (uiop:run-program
;;      (list "emacsclient" "--eval" s-exps-string))))

;; (define-command-global my/open-html-in-emacs ()
;;       "Open buffer html in Emacs."
;;       (when (equal (mode-name (current-buffer)) 'web-buffer))
;;       (with-open-file
;;        (file "/tmp/temp-nyxt.html" :direction :output
;;                                      :if-exists :supersede
;;                                      :if-does-not-exist :create)
;;        (write-string (ffi-buffer-get-document (current-buffer)) file))
;;       (eval-in-emacs
;;        `(progn
;;           (switch-to-buffer (get-buffer-create ,(render-url (url (current-buffer)))))
;;           (erase-buffer)
;;           (insert-file-contents-literally "/tmp/temp-nyxt.html")
;;           (html-mode)
;;           (indent-region (point-min) (point-max))))
;;       (delete-file "/tmp/temp-nyxt.html"))
