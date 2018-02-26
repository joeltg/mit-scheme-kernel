(import "../../../shared" print)
(import "widget" make-widget-method add-widget-method!)
(define custom-name "custom")

(define (custom-handler session widget data)
  (print "custom handler" data))

(define widget-method-custom (make-widget-method custom-name custom-handler))
(add-widget-method! widget-method-custom)
