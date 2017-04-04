(define delimiter "<IDS|MSG>")

(define args (command-line))
(assert (> (length args) 0))

(define file (open-input-file (car args)))
(define text (read-string (char-set) file))
(define json (json-decode text))

(assert (and (vector? json)
	     (= 1 (vector-length json))))

(define (ref key)
  (cdr (assq key (vector-ref json 0))))
(define control-port     (ref 'control_port))
(define shell-port       (ref 'shell_port))
(define transport        (ref 'transport))
(define signature-scheme (ref 'signature_scheme))
(define stdin-port       (ref 'stdin_port))
(define hb-port          (ref 'hb_port))
(define ip               (ref 'ip))
(define iopub-port       (ref 'iopub_port))
(define key              (ref 'key))

(define (make-endpoint port)
  (string-append transport "://" ip ":" (number->string port)))

(define control-endpoint (make-endpoint control-port))
(define shell-endpoint   (make-endpoint shell-port))
(define stdin-endpoint   (make-endpoint stdin-port))
(define hb-endpoint      (make-endpoint hb-port))
(define iopub-endpoint   (make-endpoint iopub-port))

(define endpoints (list hb-endpoint shell-endpoint control-endpoint stdin-endpoint))

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

(define (reply socket uuid parent msg-type content)
  (let ((header (make-header parent msg-type)))
    (let ((json (list header parent '() content)))
      (let ((blobs (map json-encode json)))
	(let ((hmac (make-hmac signature-scheme key blobs)))
	  (apply zmq-send-list socket uuid delimiter hmac blobs))))))

(define (pub uuid header state)
  (reply iopub-socket
	 uuid
	 header
	 "status"
	 `((execution_state . ,state))))

(define (shutdown-reply socket uuid header content)
  (reply socket uuid header "shutdown_reply" content))

(define (shutdown-request socket uuid json)
  (let ((content (get-content json))
	(header (get-header json)))
    (write-string "exiting scheme\n")
    (shutdown-reply socket uuid header content)
    (for-each zmq-socket-unbind sockets endpoints)
    (for-each zmq-socket-close sockets)
    (zmq-socket-unbind iopub-socket iopub-endpoint)
    (zmq-socket-close iopub-socket)
    (zmq-context-terminate context)
    (%exit)))

(define (inspect-request   socket uuid json) #!unspecific)
(define (complete-request  socket uuid json) #!unspecific)
(define (history-request   socket uuid json) #!unspecific)
(define (comm-info-request socket uuid json) #!unspecific)

(define routes
  `(("execute_request" . ,execute-request)
    ("inspect-request" . ,inspect-request)
    ("complete_request" . ,complete-request)
    ("history_request" . ,history-request)
    ("is_complete_request" . ,is-complete-request)
    ("comm_info_request" . ,comm-info-request)
    ("kernel_info_request" . ,kernel-info-request)
    ("shutdown_request" . ,shutdown-request)
    ("input_reply" . ,input-reply)))

(define asss (association-procedure string=? car))

(define (route-message msg-type)
  (let ((route (asss msg-type routes)))
    (if route
	(cdr route)
	(error "invalid message type"))))

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

(define ((8b-ref string) k)
  (vector-8b-ref string k))

(define (vector-ref-0 vector)
  (if (and vector (vector? vector))
      (vector-ref vector 0)
      (error "could not parse json")))

(define ((shell-fold socket) blobs len)
  (cons (zmq-receive socket len) blobs))

(define (shell-handler socket pollitem)
  (let ((blobs (reverse (fold-left (shell-fold socket) '() lengths))))
    (let ((uuid (car blobs))
	  (deli (cadr blobs))
	  (hmac (caddr blobs))
	  (json (map vector-ref-0 (map json-decode (cdddr blobs)))))
      (assert (string=? deli delimiter))
      (let ((msg-type (cdr (assq 'msg_type (car json)))))
	((route-message msg-type) socket uuid json)))))

(define control-handler shell-handler)
(define stdin-handler shell-handler)

(define (hb-handler socket pollitem)
  (zmq-send socket (zmq-receive socket hb-length)))

(define handlers (list hb-handler shell-handler control-handler stdin-handler))

(define (poll)
  (zmq-poll (car pollitems) (length pollitems) -1)
  (for-each
   (lambda (handler pollitem socket)
     (if (zmq-pollin? (zmq-pollitem-revents pollitem))
	 (handler socket pollitem)))
   handlers
   pollitems
   sockets)
  (poll))

