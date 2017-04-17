(define comm-version "~2.1.4")
(define comm-widget-target "jupyter.widget")
(define comm-module "jupyter-js-widgets")

(define (default-handler state)
  #!unspecific)

(define-structure
  (widget (constructor initialize-widget (id comm model view state)))
  (id)
  (comm)
  (model)
  (view)
  (state '())
  (handler default-handler))

(define (make-widget-model widget)
  (string-append "IPY_MODEL_" (comm-id (widget-comm widget))))

(define widget-ref (association-procedure string=? widget-id))

(define (make-widget-data model view #!optional state)
  (fold-right
    cons
    (if (default-object? state) '() state)
    `((_view_module . ,comm-module)
      (_view_name . ,view)
      (msg_throttle . 1)
      (_model_module . ,comm-module)
      (_model_module_version . ,comm-version)
      (_view_module_version . ,comm-version)
      (_model_name . ,model))))

(define (make-widget model view #!optional state)
  (let ((state (if (default-object? state) '() state)))
    (let ((data (make-widget-data model view state)))
      (let ((comm (open-comm session comm-widget-target data)))
        (let ((widget (initialize-widget (comm-id comm) comm model view state)))
          (session-add-comm! session comm)
          (session-add-widget! session widget)
          widget)))))

(define (display-widget widget)
  (let ((comm (widget-comm widget)))
    (send-comm-msg comm '((method . "display")))
    ((session-pub (comm-session comm))
      "display_data"
      `((transient)
        (data
	        (application/vnd.jupyter.widget-view+json
	          (model_id . ,(comm-id comm))))
	      (metadata)))))

(define (merge-states old new)
  (fold-right
    cons
    new
    (filter (lambda (e) (not (assq (car e) new))) old)))

(define (update-widget widget state)
  (let ((comm (widget-comm widget)))
    (send-comm-msg comm `((method . "update") (state . ,state)))
    (set-widget-state! widget (merge-states (widget-state widget) state))))

(define ((widget-updater property predicate) widget value)
  (assert (predicate value))
  (update-widget widget (list (cons property value))))