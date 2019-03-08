(import-from "../json/json-decode" json-decode)

(import-from "../zmq"
  make-zmq-context
  make-zmq-socket
  zmq-socket/bind!

  ; make-zmq-poller
  ; zmq-poller/destroy!
  ; zmq-poller/add!
  ; zmq-poller/remove!
  ; zmq-poller/wait

  zmq-poll
  zmq-pollitem/revents
  make-zmq-pollitems
  zmq-poll/event?
  receive-message
  receive-messages
  zmq-message/echo!)

(import-from "../shared" set-session-pub!)
(import-from "utils" delimiter send asss vector-ref-0)
(import-from "info" kernel-info-request)
(import-from "shutdown" shutdown-request)
(import-from "session" session-ref make-session)
(import-from "complete" is-complete-request)
(import-from "execute" execute-request)
(import-from "comm/comm" comm-info-request comm-open comm-msg)
(import-from "comm/version")
(import-from "comm/widget/widget")
(import-from "comm/widget/backbone")
(import-from "comm/widget/custom")

(define get-header car)
(define get-parent cadr)
(define get-metadata caddr)
(define get-content cadddr)

(define ((handle env) pollitem handler socket)
  (if (zmq-poll/event? 'pollin (zmq-pollitem/revents pollitem))
    (handler socket env)))

(define (inspect-request  session content reply pub . env) #!unspecific)
(define (complete-request session content reply pub . env) #!unspecific)
(define (history-request  session content reply pub . env) #!unspecific)
(define (input-reply      session content reply pub . env) #!unspecific)

(define routes
  `(("execute_request" . ,execute-request)
    ("inspect_request" . ,inspect-request)
    ("complete_request" . ,complete-request)
    ("history_request" . ,history-request)
    ("is_complete_request" . ,is-complete-request)
    ("comm_info_request" . ,comm-info-request)
    ("comm_open" . ,comm-open)
    ("comm_msg" . ,comm-msg)
    ("kernel_info_request" . ,kernel-info-request)
    ("shutdown_request" . ,shutdown-request)
    ("input_reply" . ,input-reply)))

(define (router msg-type)
  (let ((route (asss msg-type routes)))
    (if route 
      (cdr route)
	    (error "invalid message type"))))

(define ((make-reply socket identity parent signature-scheme key) msg-type content)
  (send socket identity parent msg-type signature-scheme key content))

(define ((make-pub iopub-socket identity parent signature-scheme key) msg-type content)
  (send iopub-socket identity parent msg-type signature-scheme key content))

(define ((make-handler iopub-socket get-session signature-scheme key) socket env)
  (let ((blobs (reverse (receive-messages socket))))
    (let ((identity (car blobs))
          (deli (cadr blobs))
          (hmac (caddr blobs))
          (json (map vector-ref-0 (map json-decode (map utf8->string (cdddr blobs))))))
      (assert (bytevector=? deli delimiter))
      (let ((header (get-header json))
            (content (get-content json)))
        (let ((session (get-session identity header))
              (reply (make-reply socket identity header signature-scheme key))
              (pub (make-pub iopub-socket identity header signature-scheme key))
              (route (router (cdr (assq 'msg_type header)))))
          (set-session-pub! session pub)
          (apply route session content reply pub env))))))

(define (listen
	 transport
	 ip
	 signature-scheme
	 key
	 control-port
	 shell-port
	 stdin-port
	 hb-port
	 iopub-port)

  (define (make-endpoint port)
    (string-append transport "://" ip ":" (number->string port)))

  (define control-endpoint (make-endpoint control-port))
  (define shell-endpoint   (make-endpoint shell-port))
  (define stdin-endpoint   (make-endpoint stdin-port))
  (define hb-endpoint      (make-endpoint hb-port))
  (define iopub-endpoint   (make-endpoint iopub-port))

  (define endpoints
    (list hb-endpoint shell-endpoint control-endpoint stdin-endpoint))

  (define context (make-zmq-context))

  (define hb-socket      (make-zmq-socket context 'rep))
  (define shell-socket   (make-zmq-socket context 'router))
  (define control-socket (make-zmq-socket context 'router))
  (define iopub-socket   (make-zmq-socket context 'pub))
  (define stdin-socket   (make-zmq-socket context 'router))

  (define sockets (list hb-socket shell-socket control-socket stdin-socket))
  (define pollitems (make-zmq-pollitems 4 sockets))

  ; (define poller-width (length sockets))
  ; (define poller (make-zmq-poller))
  ; (for-each
  ;   (lambda (socket)
  ;     (zmq-poller/add! poller socket 'pollin))
  ;   sockets)

  (zmq-socket/bind! iopub-socket iopub-endpoint)
  (for-each zmq-socket/bind! sockets endpoints)

  (define env
    (list context endpoints sockets iopub-endpoint iopub-socket))

  (define sessions '())

  (define (get-session identity header)
    (let ((session (cdr (assq 'session header))))
      (or (session-ref session sessions)
	  (let ((s (make-session identity session)))
	    (set! sessions (cons s sessions))
	    s))))

  (define shell-handler (make-handler iopub-socket get-session signature-scheme key))
  (define control-handler shell-handler)
  (define stdin-handler shell-handler)

  (define (hb-handler socket env)
    (zmq-message/echo! socket socket))

  (define handlers
    (list hb-handler shell-handler control-handler stdin-handler))

  ; (define poller-events (malloc (* (c-sizeof ""))))
  (let poll ()
    (zmq-poll (car pollitems) (length pollitems) -1)
    (for-each (handle env)
     pollitems
     handlers
     sockets)
    (poll)))

(export-to listen)