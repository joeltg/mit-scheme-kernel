(define sessions '())
(define none #!unspecific)

(define (get-expressions content)
  (let ((code (open-input-string (cdr (assq 'code content)))))
    (let iter ((expression (read code)) (expressions '()))
      (if (eof-object? expression)
	  expressions
	  (iter (read code) (cons expression expressions))))))

(define (evaluate session content pub)
  (fold-right
   (lambda (exp pre)
     (let ((env (session-env session)))
       (ge env)
       (eval exp env)))
   none
   (get-expressions content)))
  
(define (with-session session thunk)
  (with-error session
    (lambda ()
      (with-stdio session thunk))))

(define (execute-request session content reply pub . env)
  (pub "status" '((execution_state . "busy")))
  (session-count! session)
  (prepare-session! session pub)
  (pub "execute_input"
       `(,(assq 'code content)
	 (execution_count . ,(session-count session))))
  (execute-reply
   session reply
   (with-session session
    (lambda ()
      (execute-result session content pub))))
  (pub "status" '((execution_state . "idle"))))

(define (execute-result session content pub)
  (let ((value (evaluate session content pub)))
    (if (not (eq? value #!unspecific))
      (pub
      "execute_result"
      `((data . ((text/plain . ,(write-to-string value))))
        (metadata)
        (execution_count . ,(session-count session)))))))

(define (execute-reply session reply status)
  (reply "execute_reply"
	 `((status . ,status)
	   (execution_count . ,(session-count session))
	   (payload)
	   (user_expressions))))


