(define backbone-name "backbone")

(define (backbone-handler session widget data)
  (print "backbone handler" data)
;   (let ((state (cdr (assq 'sync_data data))))
;     (set-widget-state!
;         widget
;         (merge-states (widget-state widget) state))
; ;;     ((widget-handler widget) state)))
;     (for-each
;       (lambda (handler)
;         (with-session session
;           (lambda ()
;             ((cdr handler) state))))
;       (widget-handlers widget)))
      )

(define backbone-method (make-widget-method backbone-name backbone-handler))
(add-widget-method! backbone-method)