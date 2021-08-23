(load-after-system :nx-search-engines (nyxt-init-file "nx-search-engines/search-engines.lisp"))
(in-package :nyxt)

(define-configuration (buffer web-buffer)
    ((default-modes (append '(blocker-mode vi-normal-mode vi-insert-mode) %slot-default%))
     (search-engines (list (engines:google :shortcut "gmaps"
                                           :object :maps)
                           (engines:wikipedia :shortcut "wiki")
                           (engines:google :shortcut "goog"
                                           :safe-search nil)))))
(defun old-reddit-handler (request-data)
  (let ((url (url request-data)))
    (setf (url request-data)
          (if (search "reddit.com" (quri:uri-host url))
              (progn
                (setf (quri:uri-host url) "old.reddit.com")
                (log:info "Switching to old Reddit: ~s" (render-url url))
                url)
              url)))
  request-data)

(define-configuration web-buffer
  ((request-resource-hook
    (hooks:add-hook %slot-default% (make-handler-resource #'old-reddit-handler)))))

;; (defun old-reddit-hook (url)
;;   "Enforce old reddit."
;;   (let* ((uri (quri:uri url)))
;;     (if (search "www.reddit" (quri:uri-host uri))
;;         (progn
;;           (setf (quri:uri-host uri) "old.reddit.com")
;;           (let ((new-url (quri:render-uri uri)))
;;             (log:info "Switching to old Reddit: ~a" new-url)
;;             new-url))
;;         url)))
;; (add-to-default-list #'old-reddit-hook 'buffer 'load-hook)

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
