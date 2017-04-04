

;(define (with-sdtio port thunk)
;  (with-output-to-port port
;    (lambda ()
;      (with-input-from-port port
;	(lambda ()
;	  (with-interaction-i/o-port port thunk))))))

(define (with-stdio port thunk)
  (with-output-to-port port thunk))

(define (stdio-write-char port char)
  (reply iopub-socket
	 (stdio-port/uuid port)
	 (stdio-port/parent port)
	 "stream"
	 `((name . "stdout")
	   (text . ,(char->string char))))
  0)

(define (stdio-write-substring port string start end)
  (reply iopub-socket
	 (stdio-port/uuid port)
	 (stdio-port/parent port)
	 "stream"
	 `((name . "stdout")
	   (text . ,(substring string start end))))
  0)

;(define (stdio-read port char-set)
;  (reply stdin-socket
;	 (stdio-port/uuid port)
;	 (stdio-port/parent port)
;	 "input_request"
;	 `((prompt . "input >")
;	   (password . #f))))

(define (input-reply socket uuid json)
  (let ((header (get-header json))
	(content (get-content json)))
    (let ((environment (get-environment uuid header)))
      (write-string
       (cdr (assq 'value content))
       (cadddr environment)))))

(define (stdio-port/uuid port)
  (car (port/state port)))

(define (stdio-port/parent port)
  (cadr (port/state port)))

(define stdio-port-type
  (make-port-type
   `((write-substring ,stdio-write-substring)
     (write-char ,stdio-write-char))
   ;(port/type console-i/o-port)
   #f
   ))

(define (make-stdio uuid parent)
  (make-port stdio-port-type (list uuid parent)))

