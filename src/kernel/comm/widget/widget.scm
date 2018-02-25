(import "../../utils" asss)

(define comm-widget-name "jupyter.widget")

(define widget-methods '())
(define make-widget-method cons)
(define widget-method-name car)
(define widget-method-handler cdr)
(define (add-widget-method! widget-method)
  (set! widget-methods (cons widget-method widget-methods)))

(define (comm-widget-open session pub id data)
  (let ((comm (make-comm session comm-widget-name id))
        (widget-class (cdr (assq 'widget_class data))))
    (print "opened new widget of class" widget-class)))

(define (comm-widget-handler session pub id data)
  (let ((widget (widget-ref id (session-widgets session)))
        (method-name (cdr (assq 'method data))))
    (let ((widget-method (asss method-name widget-methods)))
      (if widget-method
        ((widget-method-handler widget-method) session widget data)
        (error "invalid widget method")))))

(define comm-target-widget
    (make-comm-target comm-widget-name comm-widget-open comm-widget-handler))
(add-comm-target! comm-target-widget)