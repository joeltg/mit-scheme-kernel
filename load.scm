

(cd "/home/joel/Documents/json.scm")
(load "json")

(cd "/home/joel/Documents/zmq.scm")
(load "zmq")

(define args (command-line))
(assert (> (length args) 0))
(define file (open-input-file (car args)))
(define text (read-string (char-set) file))
(define json (json-decode text))

(define dict (vector-ref json 0))

(define (ref key) (cdr  (assq key dict)))
(define control-port     (ref 'control_port))
(define shell-port       (ref 'shell_port))
(define transport        (ref 'transport))
(define signature-scheme (ref 'signature_scheme))
(define stdin-port       (ref 'stdin_port))
(define hb-port          (ref 'hb_port))
(define ip               (ref 'ip))
(define iopub-port       (ref 'iopub_port))
(define key              (ref 'key))

(pp "loaded config")
(pp dict)

(define endpoint
  (string-append transport "://" ip ":"))
(define control-endpoint
  (string-append endpoint (number->string control-port)))
(define shell-endpoint
  (string-append endpoint (number->string shell-port)))
(define stdin-endpoint
  (string-append endpoint (number->string stdin-port)))
(define heartbeat-endpoint
  (string-append endpoint (number->string hb-port)))
(define iopub-endpoint
  (string-append endpoint (number->string iopub-port)))

(define endpoints
  (list heartbeat-endpoint shell-endpoint control-endpoint))

(define pub-socket (make-zmq-socket context 'pub))
(zmq-socket-bind pub-socket iopub-endpoint)

(define kernel-info
  '((protocol_version . "5.1")
    (implementation . "ischeme")
    (implementation_version . "0.0.1")
    (language_info . ((name . "MIT Scheme")
		      (version . "9.2.1")
		      (mimetype . "text/scheme")
		      (file_extension . ".scm")
		      (pygments_lexer . "mit-scheme")
		      (codemirror_mode . "scheme")))
    (banner . "MIT Scheme Kernel")
    (help_links . #(((text . "GitHub")
		     (url . "https://github.com/joeltg/ischeme"))))))

(define context (make-zmq-context))

(define (get-msg-id)
  (number->string (random (expt 2 128)) 16))

(define (pad n l)
  (let ((s (number->string n)))
    (string-append (make-string (- l (string-length s)) #\0) s)))

(define (get-date)
  (let ((time (global-decoded-time)))
    (string-append
     (pad (decoded-time/year time) 4)
     "-"
     (pad (decoded-time/month time) 2)
     "-"
     (pad (decoded-time/day time) 2)
     "T"
     (pad (decoded-time/hour time) 2)
     ":"
     (pad (decoded-time/minute time) 2)
     ":"
     (pad (decoded-time/second time) 2)
     "Z")))

(define (reply socket uuid header msg-type content)
  (let ((username (cdr (assq 'username header)))
	(session (cdr (assq 'session header))))
    (for-each
     (lambda (blob)
       (zmq-send socket blob))
     (list
      (
    (zmq-send
     socket
     (json-encode
      `((header . ((msg_id   . ,(get-msg-id))
		   (username . ,username)
		   (session  . ,session)
		   (date     . ,(get-date))
		   (msg_type . ,msg-type)
		   (version  . "5.1")))
	(parent_header . ,header)
	(metadata . ())
	(content . ,content)
	(buffers . #()))))))

(define (pub header state)
  (reply pub-socket header "status" `((execution_state . ,state))))

(define (execute-request header content socket)
  #!unspecific)

(define (inspect-request header content socket)
  #!unspecific)

(define (complete-request header content socket)
  #!unspecific)

(define (history-request header content socket)
  #!unspecific)

(define (is-complete-request header content socket)
  #!unspecific)

(define (comm-info-request header content socket)
  #!unspecific)

(define (kernel-info-request header content socket)
  (pp "kernel info request")
  (pub 'busy)
  (kernel-info-reply header content socket))

(define (kernel-info-reply header content socket)
  (reply socket header "kernel_info_reply" kernel-info))

(define (shutdown-request header content socket)
  #!unspecific)

(define (input-request header content socket)
  #!unspecific)

(define (handle-shell socket content meta parent header)
  (pp "handling shell")
  (define msg-type (cdr (assq 'msg_type header)))
  (cond ((string=? msg-type "execute_request")
	 (execute-request header content socket))
	((string=? msg-type "inspect-request")
	 (inspect-request header content socket))
	((string=? msg-type "complete_request")
	 (complete-request header content socket))
	((string=? msg-type "history_request")
	 (history-request header content socket))
	((string=? msg-type "is_complete_request")
	 (is-complete-request header content socket))
	((string=? msg-type "comm_info_request")
	 (comm-info-request header content socket))
	((string=? msg-type "kernel_info_request")
	 (kernel-info-request header content socket))
	((string=? msg-type "shutdown_request")
	 (shutdown-request header content socket))
	((string=? msg-type "input_request")
	 (input-request header content socket))))

(define shell-lengths
  (list 33 9 64 2048 2048 1024 4096))

(define ((shell-fold socket) blobs len)
  (cons (zmq-receive socket len) blobs))

(define (vector-ref-0 vector)
  (vector-ref vector 0))

(define (shell-handler socket pollitem size)
  (pp "shell!")
  (let ((blobs (fold-left (shell-fold socket) '() shell-lengths)))
    (let ((json (map json-decode (list-head blobs 4))))
      (apply handle-shell socket (map vector-ref-0 json)))))

(define ((8b-ref string) k) (vector-8b-ref string k))

(define (heartbeat-handler socket pollitem size)
  (pp "heartbeat!")
  (let ((message (zmq-receive socket size)))
    (zmq-send socket message)))

(define handlers (list heartbeat-handler shell-handler shell-handler))

(define (get-pollitem pollitems index)
  (alien-byte-increment
   pollitems
   (* index zmq-poll-size)
   'zmq_pollitem_t))

(define ((make-pollitem pollitems) i socket events)
  (let ((pollitem (get-pollitem pollitems i)))
    (c->= pollitem "zmq_pollitem_t socket" socket)
    (c->= pollitem "zmq_pollitem_t events" events)
    pollitem))

(define (make-pollitems n sockets event-lists)
  (let ((pollitems (malloc (* n zmq-poll-size) 'zmq_pollitem_t)))
    (map
     (make-pollitem pollitems)
     (iota n)
     sockets
     (map make-zmq-poll-event event-lists))))

(define heartbeat-socket (make-zmq-socket context 'rep))
(define shell-socket (make-zmq-socket context 'router))
(define control-socket (make-zmq-socket context 'router))

(define sockets (list heartbeat-socket shell-socket control-socket))
(define events (make-list 3 '(pollin)))
(define pollitems (make-pollitems 3 sockets events))

(define heartbeat-pollitem (car pollitems))
(define shell-pollitem (cadr pollitems))
(define control-pollitem (caddr pollitems))

(for-each zmq-socket-bind sockets endpoints)
(define size (expt 2 10))

(define (poll)
  (zmq-poll (car pollitems) (length pollitems) -1)
  (for-each
   (lambda (handler pollitem socket)
     (if (zmq-pollin? (zmq-pollitem-revents pollitem))
	 (handler socket pollitem size)))
   handlers
   pollitems
   sockets))

(let iter ()
  (poll)
  (iter))

(pp "end of file")
