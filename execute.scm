
(define sessions '())
(define none #!unspecific)

(define (execute-request socket uuid json)
  (let ((header (get-header json))
	(content (get-content json)))
    (pub uuid header "busy")
    (execute-reply socket uuid header content)
    (pub uuid header "idle")))

(define (get-expressions content)
  (let ((code (open-input-string (cdr (assq 'code content)))))
    (let iter ((expression (read code)) (expressions '()))
      (if (eof-object? expression)
	  expressions
	  (iter (read code) (cons expression expressions))))))

(define session-env cadr)
(define session-count caddr)
(define session-stdio cadddr)
(define (session-count! session)
  (set-car! (cddr session) (+ 1 (caddr session))))

(define make-env make-top-level-environment)

(define (get-session uuid header)
  (let ((s (cdr (assq 'session header))))
    (or (asss s sessions)
	(let ((session (list s (make-env) 0 (make-stdio uuid header))))
	  (set! sessions (cons session sessions))
	  session))))

(define (evaluate session expressions)
  (fold-right
   (lambda (exp pre)
     (session-count! session)
     (eval exp (session-env session)))
   none expressions))

;; execute return the content for an execute-reply
;; which is either {status "ok"} or {status "error"}
;; and maybe dispatches execute-result separately
(define (execute socket uuid header content)
  (let ((session (get-session uuid header)))
    (call-with-current-continuation
     (lambda (kappa)
       (with-error
	kappa session uuid header
	(lambda ()
	  (let ((expressions (get-expressions content)))
	    (with-stdio
	     (session-stdio session)
	     (lambda ()
	       (execute-result session uuid header
			       (evaluate session expressions))
	       `((status . "ok")
		 (execution_count . ,(session-count session))
		 (payload)
		 (user_expressions))
	       )
	     )
	    )))))))

(define (execute-reply socket uuid header content)
  (reply socket uuid header "execute_reply"
	 (execute socket uuid header content)))

(define (execute-result session uuid header value)
  (reply iopub-socket uuid header "execute_result"
   `((data . ((text/plain . ,(write-to-string value))))
     (metadata)
     (execution_count . ,(session-count session)))))
