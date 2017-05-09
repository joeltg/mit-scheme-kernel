;; Util
(define (make-msg-id)
  (number->string (random (expt 2 128)) 16))

(define (valid-arity? procedure n)
  (let ((arity (procedure-arity procedure)))
    (and 
      (>= n (procedure-arity-min arity))
      (<= n (procedure-arity-max arity)))))

;; Session
(define (default-pub . args)
  (apply print "default pub!" args))

(define-structure
  (session (constructor initialize-session (uuid)))
  (uuid)
  (pub default-pub)
  (count 0)
  (stdio #!unspecific)
  (env (extend-top-level-environment shared-env))
  (comms '())
  (widgets '()))

(define (session-add-comm! session comm)
  (set-session-comms! session (cons comm (session-comms session))))

(define (session-add-widget! session widget)
  (set-session-widgets! session (cons widget (session-comms session))))

;; Comm
(define-structure
  (comm (constructor initialize-comm (session target #!optional id)))
  (session)
  (target)
  (id (make-msg-id)))

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

;; Widget
(define-structure
  (widget (constructor initialize-widget (id comm model view state)))
  (id)
  (comm)
  (model)
  (view)
  (handler (lambda args #!unspecific))
  (state '())
  (handlers '()))

(define widget-ref (association-procedure string=? widget-id))

(define (merge-states old new)
  (fold-right
    cons
    new
    (filter (lambda (e) (not (assq (car e) new))) old)))