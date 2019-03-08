;; ZeroMQ for MIT Scheme

(import-from "zmq-constants"
  zmq-context-options
  zmq-socket-types
  zmq-socket-options
  zmq-poll-events
  zmq-message-options)

(define ld-library-path "/usr/local/lib/mit-scheme-x86-64/:/usr/local/lib/")
(set-environment-variable! "LD_LIBRARY_PATH" ld-library-path)

(load-option 'ffi)
(c-include "zmq")

(define message-size 64)
(define (make-message)
  (malloc message-size 'zmq_msg_t))

(define (make-pointer)
  (copy-alien null-alien))

;; Errors
(define (zmq-error message)
  (error message
	  (c-peek-cstring
      (c-call "zmq_strerror"
        (make-pointer)
        (c-call "zmq_errno")))))

;; Contexts
(define (make-zmq-context)
  (define context (c-call "zmq_ctx_new" (make-pointer)))
  (if (alien-null? context)
    (zmq-error "make-zmq-context")
    context))

(define (zmq-context/terminate! context)
  (if (= -1 (c-call "zmq_ctx_term" context))
    (zmq-error "zmq-context/terminate!")))

(define (zmq-context/shutdown! context)
  (if (= -1 (c-call "zmq_ctx_shutdown" context))
    (zmq-error "zmq-context/shutdown!")))

(define (zmq-context/destroy! context)
  (if (= -1 (c-call "zmq_ctx_destroy" context))
    (zmq-error "zmq-context/destroy!")))

(define (zmq-context/get context option)
  (define option-value (cadr (assq option zmq-context-options)))
  (define result (c-call "zmq_ctx_get" context option-value))
  (if (= -1 result)
    (zmq-error "zmq-context/get")
    result))

(define (zmq-context/set! context option value)
  (define option-value (cadr (assq option zmq-context-options)))
  (define result (c-call "zmq_ctx_get" context option-value value))
  (if (= -1 result)
    (zmq-error "zmq-context/set!")
    result))

;; Sockets
(define (make-zmq-socket context type)
  (define type-value (cadr (assq type zmq-socket-types)))
  (define socket (c-call "zmq_socket" (make-pointer) context type-value))
  (if (alien-null? socket)
    (zmq-error "make-zmq-socket")
    socket))

(define (zmq-socket/close! socket)
  (if (= -1 (c-call "zmq_close" socket))
    (zmq-error "zmq-socket/close!")))

(define (zmq-socket/connect! socket endpoint)
  (if (= -1 (c-call "zmq_connect" socket endpoint))
    (zmq-error "zmq-socket/connect!")))

(define (zmq-socket/disconnect! socket endpoint)
  (if (= -1 (c-call "zmq_disconnect" socket endpoint))
    (zmq-error "zmq-socket/disconnect!")))

(define (zmq-socket/bind! socket endpoint)
  (if (= -1 (c-call "zmq_bind" socket endpoint))
    (zmq-error "zmq-socket/bind!")))

(define (zmq-socket/unbind! socket endpoint)
  (if (= -1 (c-call "zmq_unbind" socket endpoint))
    (zmq-error "zmq-socket/unbind!")))

(define (zmq-socket/monitor! socket endpoint events)
  (define events-value (fold + 0 events))
  (if (= -1 (c-call "zmq_socket_monitor" socket endpoint events-value))
    (zmq-error "zmq-socket/monitor!")))

; (define (zmq-socket/get socket option)
;   )

; (define (zmq-socket/set! socket option value)
;   )

(define (zmq-socket-option option)
  (cadr (assq option zmq-socket-options)))

(define (zmq-socket/send socket . messages)
  (fold-left
    (lambda (rest message)
      (define flag (if (null? rest) 0 2))
      (define data (->bytevector message))
      (define len (bytevector-length data))
      (define buf (bytevector->buffer data len))
      (if (= -1 (c-call "zmq_send" socket buf len flag))
        (zmq-error "zmq-socket/send")
        (if (null? rest) #!unspecific (cdr rest))))
    (if (null? messages) #!unspecific (cdr messages))
    messages))

(define (zmq-socket/receive socket len . flags)
  (define flag (fold + 0 (map zmq-socket-option flags)))
  (define buf (malloc len 'char))
  (define result (c-call "zmq_recv" socket buf len flag))
  (if (= -1 result)
    (zmq-error "zmq-socket/receive")
    (buffer->bytevector buf result)))

;; Messages
(define (zmq-message/close! message)
  (if (= -1 (c-call "zmq_msg_close" message))
    (zmq-error "zmq-message/close!")))
  
(define (zmq-message/copy dest src)
  (if (= -1 (c-call "zmq_msg_copy" dest src))
    (zmq-error "zmq-message/copy")))

(define (zmq-message/move dest src)
  (if (= -1 (c-call "zmq_msg_move" dest src))
    (zmq-error "zmq-message/move")))

(define (zmq-message/size message)
  (define size (c-call "zmq_msg_size" message))
  (if (= -1 result)
    (zmq-error "zmq-message/size")
    size))

(define (zmq-message/more? message)
  (= 1 (c-call "zmq_msg_more" message)))

(define (zmq-message/init! message)
  (if (= -1 (c-call "zmq_msg_init" message))
    (zmq-error "zmq-message/init")))

(define (zmq-message/init-size! message size)
  (if (= -1 (c-call "zmq_msg_init_size" message size))
    (zmq-error "make-zmq-message/msg_init_size")))

(define (zmq-message/data message)
  (define data (c-call "zmq_msg_data" (make-pointer) message))
  (if (alien-null? data)
    (zmq-error "zmq-message/data")
    data))

(define (zmq-message/receive message socket)
  (define size (c-call "zmq_msg_recv" message socket 0))
  (if (= -1 size)
    (zmq-error "zmq-message/receive")
    size))

(define (zmq-message/echo! input output)
  (let ((message (make-message)))
    (zmq-message/init! message)
    (if (= -1 (c-call "zmq_msg_recv" message input 0))
      (zmq-error "zmq-message/echo! zmq_msg_recv")
      (if (= -1 (c-call "zmq_msg_send" message output 0))
        (zmq-error "zmq-message/echo! zmq_msg_send")))))

(define (make-zmq-message #!optional data)
  (define message (make-message))
  (define size (bytevector-length data))
  (if (or (default-object? data) (= 0 size))
    (begin
      (zmq-message/init! message)
      message)
    (begin
      (zmq-message/init-size! message size)
      (let iter ((i 0) (cell (zmq-message/data message)))
        (if (< i size)
          (begin
            (c->= cell "char" (bytevector-u8-ref data i))
            (alien-byte-increment! cell 1 'char)
            (iter (1+ i) cell))
          message)))))

;; Polling

#|
(define (zmq-poll-event event)
  (cadr (assq event zmq-poll-events)))

(define (make-zmq-poller)
  (c-call "zmq_poller_new" (make-pointer)))

(define (zmq-poller/destroy! poller)
  (if (= -1 (c-call "zmq_poller_destroy" poller))
    (zmq-error "zmq-poller/destroy!")))
  
(define (zmq-poller/add! poller socket . events)
  (define event (fold + 0 (map zmq-poll-event events)))
  (if (= -1 (c-call "zmq_poller_add" poller socket null-alien event))
    (zmq-error "zmq-poller/add!")))

(define (zmq-poller/remove! poller socket)
  (if (= -1 (c-call "zmq_poller_remove" poller socket))
    (zmq-error "zmq-poller/remove!")))

(define (zmq-poller/wait poller events count)
  (pp "wow"))
|#

(define zmq-poll-size (c-sizeof "zmq_pollitem_t"))
(define zmq-poll-event 1)

(define (get-zmq-pollitem alien i)
  (alien-byte-increment alien (* i zmq-poll-size) 'zmq_pollitem_t))

(define ((make-zmq-pollitem pollitems) i socket)
  (let ((pollitem (get-zmq-pollitem pollitems i)))
    (c->= pollitem "zmq_pollitem_t socket" socket)
    (c->= pollitem "zmq_pollitem_t events" zmq-poll-event)
    pollitem))

(define (make-zmq-pollitems n sockets)
  (map
    (make-zmq-pollitem
      (malloc (* n zmq-poll-size) 'zmq_pollitem_t))
    (iota n)
    sockets))

(define (zmq-poll/event? event revents)
  (odd? (quotient revents (cadr (assq event zmq-poll-events)))))

(define (zmq-pollitem/revents pollitem)
  (c-> pollitem "zmq_pollitem_t revents"))

(define (zmq-poll items nitems timeout)
  (c-call "zmq_poll" items nitems timeout))

(define (->bytevector data)
  (cond
    ((bytevector? data) data)
    ((string? data) (string->utf8 data))
    (else (error "Unrecognized input"))))

(define (bytevector->buffer data len)
  (define buf (malloc len 'char))
  (let iter ((i 0) (elm (copy-alien buf)))
    (if (< i len)
      (let ((char (bytevector-u8-ref data i)))
        (c->= elm "char" char)
        (alien-byte-increment! elm 1 'char)
        (iter (1+ i) elm))
      buf)))

(define (buffer->bytevector buf len)
  (define data (make-bytevector len))
  (let iter ((i 0) (elm (copy-alien buf)))
    (if (< i len)
      (let ((char (c-> elm "char")))
        (bytevector-u8-set! data i char)
        (alien-byte-increment! elm 1 'char)
        (iter (1+ i) elm))
      data)))

(define (receive-message socket)
  (define message (make-message))
  (zmq-message/init! message)
  (define size (zmq-message/receive message socket))
  (define cell (zmq-message/data message))
  (define data (make-bytevector size))
  (let iter ((i 0))
    (if (or (>= i size) (alien-null? cell))
      (begin
        (zmq-message/close! message)
        data)
      (begin
        (bytevector-u8-set! data i (c-> cell "char"))
        (alien-byte-increment! cell 1 'char)
        (iter (1+ i))))))

(define (receive-messages socket)
  (let next ((message (make-message)) (messages '()))
    (c-call "zmq_msg_init" message)
    (zmq-message/init! message)
    (define size (zmq-message/receive message socket))
    (define cell (zmq-message/data message))
    (define data (make-bytevector size))
    (let iter ((i 0))
      (if (or (>= i size) (alien-null? cell))
        (let ((more (zmq-message/more? message))
              (messages (cons data messages)))
          (zmq-message/close! message)
          (if more (next (make-message) messages) messages))
        (begin
          (bytevector-u8-set! data i (c-> cell "char"))
          (alien-byte-increment! cell 1 'char)
          (iter (1+ i)))))))

(export-to
  make-zmq-context
  zmq-context/terminate!
  make-zmq-socket
  zmq-socket/bind!
  zmq-socket/unbind!
  zmq-socket/close!
  ; make-zmq-poller
  ; zmq-poller/destroy!
  ; zmq-poller/add!
  ; zmq-poller/remove!
  ; zmq-poller/wait
  make-zmq-pollitems
  zmq-pollitem/revents
  zmq-poll/event?
  zmq-poll
  zmq-socket/send
  receive-message
  receive-messages
  zmq-message/echo!)