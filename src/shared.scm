(define shared-env (the-environment))

;; Util
(define (make-id)
  (number->string (random (expt 2 128)) 16))

(define (print . args)
  (for-each (lambda (arg) (pp arg (console-i/o-port))) args))

;; Session
(define (default-pub . args)
  (apply print "default pub!" args))

(define-structure
  (session (constructor initialize-session (identity id)))
  (identity)
  (id)
  (pub default-pub)
  (count 0)
  (stdio #!unspecific)
  (env (extend-top-level-environment shared-env))
  (comms '())
  (widgets '()))

(define (session-add-comm! session comm)
  (set-session-comms! session (cons comm (session-comms session))))

(define (session-add-widget! session widget)
  (set-session-widgets! session (cons widget (session-widgets session))))

;; Comm
(define-structure
  (comm (constructor initialize-comm (session target #!optional id)))
  (session)
  (target)
  (id (make-id)))

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

; ;; Widget
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

(export-to
  make-id
  print
  initialize-session
  session-id
  session-pub
  set-session-pub!
  session-count
  set-session-count!
  session-stdio
  set-session-stdio!
  session-env
  session-comms
  session-widgets
  session-add-comm!
  session-add-widget!
  comm-session
  comm-target
  comm-id
  make-comm
  open-comm
  send-comm-msg
  initialize-widget
  widget-comm
  widget-model
  widget-ref
  widget-handler
  set-widget-handler!
  widget-state
  set-widget-state!
  widget-handlers
  set-widget-handlers!
  merge-states)