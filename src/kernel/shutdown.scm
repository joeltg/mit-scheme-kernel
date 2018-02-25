(import "../zmq"
  zmq-socket-unbind
  zmq-socket-close
  zmq-context-terminate)

(define (shutdown-reply reply content)
  (reply "shutdown_reply" content))

(define (shutdown-request
	 session content reply pub
	 context endpoints sockets iopub-endpoint iopub-socket)
    (write-string "exiting scheme\n" console-i/o-port)
    (shutdown-reply reply content)
    (for-each zmq-socket-unbind sockets endpoints)
    (for-each zmq-socket-close sockets)
    (zmq-socket-unbind iopub-socket iopub-endpoint)
    (zmq-socket-close iopub-socket)
    (zmq-context-terminate context)
    (%exit))

(export shutdown-request)