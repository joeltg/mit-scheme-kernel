
(define version "5.1.0")

(define asss (association-procedure string=? car))

(define (make-msg-id)
  (number->string (random (expt 2 128)) 16))

(define (pad n l)
  (let ((s (number->string n)))
    (string-append
     (make-string (- l (string-length s)) #\0) s)))

(define (make-date)
  (let ((time (global-decoded-time)))
    (string-append
     (pad (decoded-time/year   time) 4) "-"
     (pad (decoded-time/month  time) 2) "-"
     (pad (decoded-time/day    time) 2) "T"
     (pad (decoded-time/hour   time) 2) ":"
     (pad (decoded-time/minute time) 2) ":"
     (pad (decoded-time/second time) 2) "Z")))

(define (make-header parent msg-type)
  (let ((username (cdr (assq 'username parent)))
	(session (cdr (assq 'session parent))))
    `((msg_id . ,(make-msg-id))
      (username . ,username)
      (session . ,session)
      (date . ,(make-date))
      (msg_type . ,msg-type)
      (version . ,version))))

(define (make-hmac scheme key blobs)
  (cond
   ((string=? scheme "hmac-sha256")
    (let ((concat (apply string-append blobs))
	  (sha256 (string-append "openssl sha256 -hmac " key))
	  (stdout (open-output-string)))
      (run-shell-command sha256
			 'input (open-input-string concat)
			 'output stdout)
      (let ((hmac (get-output-string stdout)))
	(substring hmac
		   (- (string-length hmac) 64 1)
		   (- (string-length hmac) 1)))))
   (else (warn "signature scheme not recognized") "")))
