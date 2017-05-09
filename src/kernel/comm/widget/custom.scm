(define custom-name "custom")

(define (custom-handler session widget data)
  (let ((content (cdr (assq 'content data)))
        (handler (widget-handler widget)))
    (with-session session
      (lambda ()
        (handler content)))))

(define widget-method-custom (make-widget-method custom-name custom-handler))
(add-widget-method! widget-method-custom)
