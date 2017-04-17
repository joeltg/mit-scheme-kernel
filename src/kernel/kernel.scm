(define delimiter "<IDS|MSG>")

(define get-header car)
(define get-parent cadr)
(define get-metadata caddr)
(define get-content cadddr)

(define hb-length 4)
(define uuid-length 33)
(define delimiter-length (string-length delimiter))
(define signature-length 64)
(define header-length 256)
(define parent-length 256)
(define metadata-length 2)
(define content-length 4096)
(define lengths
  (list uuid-length
	delimiter-length
	signature-length
	header-length
	parent-length
	metadata-length
	content-length))

(define ((handle env) pollitem handler socket)
  (if (zmq-pollin? (zmq-pollitem-revents pollitem))
      (handler socket env)))

(define ((shell-fold socket) blobs len)
  (cons (zmq-receive socket len) blobs))

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

(define ((make-reply socket uuid parent) msg-type content)
  (send socket uuid parent msg-type content))

(define ((make-pub iopub-socket uuid parent) msg-type content)
  (send iopub-socket uuid parent msg-type content))

(define ((make-handler iopub-socket get-session) socket env)
  (let ((blobs (reverse (fold-left (shell-fold socket) '() lengths))))
    (let ((uuid (car blobs))
	        (deli (cadr blobs))
	        (hmac (caddr blobs))
	        (json (map vector-ref-0 (map json-decode (cdddr blobs)))))
      (assert (string=? deli delimiter))
      (let ((content (get-content json))
	          (header (get-header json)))
        (let ((session (get-session uuid header))
              (reply (make-reply socket uuid header))
              (pub (make-pub iopub-socket uuid header)))
          (set-session-pub! session pub)
          (apply (router (cdr (assq 'msg_type header)))
            session content reply pub env))))))

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

  (zmq-socket-bind iopub-socket iopub-endpoint)
  (for-each zmq-socket-bind sockets endpoints)

  (define env
    (list context endpoints sockets iopub-endpoint iopub-socket))

  (define sessions '())

  (define (get-session uuid header)
    (let ((session (cdr (assq 'session header))))
      (or (session-ref session sessions)
	        (let ((s (make-session uuid)))
	          (set! sessions (cons s sessions))
	          s))))

  (define shell-handler (make-handler iopub-socket get-session))
  (define control-handler shell-handler)
  (define stdin-handler shell-handler)

  (define (hb-handler socket env)
    (zmq-send socket (zmq-receive socket hb-length)))

  (define handlers
    (list hb-handler shell-handler control-handler stdin-handler))

  (let poll ()
    (zmq-poll (car pollitems) (length pollitems) -1)
    (for-each (handle env)
     pollitems
     handlers
     sockets)
    (poll)))
