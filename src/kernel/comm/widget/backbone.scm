(import-from "../../../shared" print merge-states widget-handlers widget-state set-widget-state!)
(import-from "widget" make-widget-method add-widget-method!)
(define backbone-name "backbone")

(define (backbone-handler session widget data)
  (let ((state (cdr (assq 'sync_data data))))
    (set-widget-state!
        widget
        (merge-states (widget-state widget) state))
    (for-each
      (lambda (handler)
        ((cdr handler) state))
      (widget-handlers widget))))

(define backbone-method (make-widget-method backbone-name backbone-handler))
(add-widget-method! backbone-method)