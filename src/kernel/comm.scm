(define comm-version "~2.1.4")
(define comm-widget-target "jupyter.widget")
(define comm-version-target "jupyter.widget.version")

(define widget-ref 
  (association-procedure 
    string=? 
    (lambda (widget)
      ((record-accessor (record-type-descriptor widget) 'id) widget))))

(define (merge-states old new)
  (fold-right
    cons
    new
    (filter (lambda (e) (not (assq (car e) new))) old)))

(define (widget-handler widget state)
  (let ((type (record-type-descriptor widget)))
    ((record-modifier type 'state)
      widget
      (merge-states ((record-accessor type 'state) widget) state))
    (((record-accessor type 'handler) widget) state)))

(define-structure
  (comm (constructor initialize-comm (session target #!optional id)))
  (session)
  (target)
  (id (make-msg-id)))

(define comm-ref (association-procedure string=? comm-id))

(define (make-comm session target #!optional id)
  (let ((comm (initialize-comm session target id)))
    (session-add-comm! session comm)
    comm))

(define (make-comm-content comm #!optional data)
  `((comm_id . ,(comm-id comm))
    (target_name . ,(comm-target comm))
    (target_module . #!unspecific)
    (data . ,(if (default-object? data) '() data))))

(define (open-comm session target #!optional data)
  (let ((comm (make-comm session target)))
    ((session-pub session)
      "comm_open"
      (make-comm-content comm data))
    comm))

(define (send-comm-msg comm data)
  ((session-pub (comm-session comm))
    "comm_msg"
    `((comm_id . ,(comm-id comm))
      (target_name . ,(comm-target comm))
      (target_module . #!unspecific)
      (data . ,data))))

(define (comm-info-request session content reply pub . env)
  (pub "comm_info_reply" '((status . "ok") (comms))))

(define (comm-open-version session pub id data)
  (let ((comm (make-comm session comm-version-target id)))
    (send-comm-msg comm `((version . ,comm-version)))))

(define (comm-open-widget session pub id data)
  (let ((comm (make-comm session comm-widget-target id)))
    (let ((widget-class (cdr (assq 'widget_class data))))
      (print "opened new widget of class" widget-class))))

(define (comm-open session content reply pub . env)
  (let ((comm-id (cdr (assq 'comm_id content)))
	(data (cdr (assq 'data content)))
	(target-name (cdr (assq 'target_name content))))
    (cond ((string=? comm-version-target target-name)
	   (comm-open-version session pub comm-id data))
	  ((string=? comm-widget-target target-name)
	   (comm-open-widget session pub comm-id data))
	  (else (warn "invalid widget target")))))

(define (comm-msg-version session pub id data)
  (if (cdr (assq 'validated data))
      (print "widget comms validated")
      (warn "invalid jupter widget version")))

(define (comm-msg-widget session pub id data)
  (let ((widget (widget-ref id (session-widgets session))))
    (cond 
      ((string=? "backbone" (cdr (assq 'method data)))
        (widget-handler widget (cdr (assq 'sync_data data))))
      (else #!unspecific))))

(define (comm-msg session content reply pub . env)
  (pub "status" '((execution_state . "busy")))
  (let ((id (cdr (assq 'comm_id content)))
	      (data (cdr (assq 'data content))))
    (let ((target (comm-target (comm-ref id (session-comms session)))))
      (cond 
        ((string=? comm-version-target target)
	        (comm-msg-version session pub id data))
	      ((string=? comm-widget-target target)
	        (comm-msg-widget session pub id data)))))
  (pub "status" '((execution_state . "idle"))))
