
(define (with-stdio session reply pub thunk)
  (with-output-to-port (session/stdio session) thunk))

(define (stdio-write-char port char)
  ((port/state port)
   "stream"
   `((name . "stdout")
     (text . ,(char->string char))))
  0)

(define (stdio-write-substring port string start end)
  ((port/state port)
   "stream"
   `((name . "stdout")
     (text . ,(substring string start end))))
  0)

(define stdio-port-type
  (make-port-type
   `((write-substring ,stdio-write-substring)
     (write-char ,stdio-write-char))
   #f))

(define (make-stdio pub)
  (make-port stdio-port-type pub))

