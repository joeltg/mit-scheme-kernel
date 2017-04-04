;; ZeroMQ for MIT Scheme

(define ld-library-path "/usr/local/lib/mit-scheme-x86-64/:/usr/local/lib/")
(set-environment-variable! "LD_LIBRARY_PATH" ld-library-path)

(load-option 'ffi)
(c-include "zmq")

;; Errors
(define (zmq-error message)
  (error message
	 (c-peek-cstring
	  (c-call "zmq_strerror"
		  (malloc 8 '(* (const char)))
		  (c-call "zmq_errno")))))

;; Contexts
(define (make-zmq-context)
  (let ((context (c-call "zmq_ctx_new" (malloc 8 '(* void)))))
    (if (alien-null? context)
      (zmq-error "could not make context")
      context)))

(define (zmq-context-terminate context)
  (if (= -1 (c-call "zmq_ctx_term" context))
      (zmq-error "could not terminate context")))

(define (make-zmq-socket context type)
  (let ((a (malloc 8 '(* void)))
	(t (get-zmq-socket-type type)))
    (let ((socket (c-call "zmq_socket" a context t)))
      (if (alien-null? socket)
	  (zmq-error "could not make socket")
	  socket))))

(define (zmq-socket-close socket)
  (if (= -1 (c-call "zmq_close" socket))
    (zmq-error "could not close socket")))

(define (zmq-socket-bind socket address)
  (if (= -1 (c-call "zmq_bind" socket address))
    (zmq-error "could not bind socket")))

(define (zmq-socket-unbind socket address)
  (if (= -1 (c-call "zmq_unbind" socket address))
    (zmq-error "could not unbind socket")))

(define (get-zmq-socket-type type)
  (let ((t (assq type zmq-socket-types)))
    (if t (cadr t) (error "invalid socket type"))))

(define zmq-socket-types
  '((pair   0)
    (pub    1)
    (sub    2)
    (req    3)
    (rep    4)
    (dealer 5)
    (router 6)
    (pull   7)
    (push   8)
    (xpub   9)
    (xsub   10)
    (stream 11)))

;; Polling
(define zmq-poll-size (c-sizeof "zmq_pollitem_t"))
;(define zmq-poll-events (+ 1 2))
(define zmq-poll-events 1)

(define (get-zmq-pollitem alien i)
  (alien-byte-increment
   alien (* i zmq-poll-size) 'zmq_pollitem_t))

(define ((make-zmq-pollitem pollitems) i socket)
  (let ((pollitem (get-zmq-pollitem pollitems i)))
    (c->= pollitem "zmq_pollitem_t socket" socket)
    (c->= pollitem "zmq_pollitem_t events" zmq-poll-events)
    pollitem))

(define (make-zmq-pollitems n sockets)
  (let ((pollitems (malloc (* n zmq-poll-size) 'zmq_pollitem_t)))
    (map
     (make-zmq-pollitem pollitems)
     (iota n)
     sockets)))

(define (zmq-pollin? revents)
  (= (modulo revents 2) 1))

(define (zmq-pollitem-revents pollitem)
  (c-> pollitem "zmq_pollitem_t revents"))

(define (zmq-poll items nitems timeout)
  (c-call "zmq_poll" items nitems timeout))

;; Messages
(define zmq-buffer-size vector-8b-length)

(define (make-zmq-buffer data)
  (let ((len (zmq-buffer-size data)))
    (let ((buf (malloc len 'char)))
      (let iter ((k 0) (elm (copy-alien buf)))
	(if (< k len)
	    (let ((char (vector-8b-ref data k)))
	      (c->= elm "char" char)
	      (alien-byte-increment! elm 1 'char)
	      (iter (+ k 1) elm))
	    buf)))))

(define (parse-zmq-buffer buf len ret)
  (if (> ret len) (warn "message truncated"))
  (let ((data (make-string (min len ret))))
    (let iter ((k 0) (elm buf))
      (if (< k (min len ret))
	  (let ((char (c-> elm "char")))
	    (vector-8b-set! data k char)
	    (alien-byte-increment! elm 1 'char)
	    (iter (+ k 1) elm))
	  data))))

(define (zmq-send socket data)
  (c-call "zmq_send"
	  socket
	  (make-zmq-buffer data)
	  (vector-8b-length data)
	  0))

(define (zmq-send-list socket . messages)
  (for-each
   (lambda (data flag)
     (c-call "zmq_send"
	     socket
	     (make-zmq-buffer data)
	     (vector-8b-length data)
	     flag))
   messages
   (reverse (cons 0 (make-list (- (length messages) 1) 2)))))

(define (zmq-receive socket size)
  (let ((buffer (malloc size 'char)))
    (let ((ret (c-call "zmq_recv" socket buffer size 0)))
      (if (= -1 ret)
	  (zmq-error "could not receive message")
	  (parse-zmq-buffer buffer size ret)))))
