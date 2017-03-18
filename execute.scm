(define environments '())
(define none #!unspecific)

(define (execute-request socket uuid json)
  (pp "execute request")
  (pp json)
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

(define (new-environment)
  (make-top-level-environment))

(define (get-environment header)
  (let ((session (cdr (assq 'session header))))
    (or (asss session environments)
	(let ((environment (list session (new-environment) 0)))
	  (set! environments (cons environment environments))
	  environment))))

(define ((evaluate environment) expression pre)
  (pp (list "evaluating" expression))
  (set-car! (cddr environment) (+ 1 (caddr environment)))
  (eval expression (cadr environment)))

(define (error-hook condition)
  (pp "condition!")
  (invoke-restart (find-restart 'jupyter-error)
		  (condition/report-string condition)))

(define ((effector kappa uuid header) report)
  (pp "effector!")
  (let ((environment (get-environment header)))
    (error-result uuid header (caddr environment) report)
    (kappa (cons "error" (caddr environment)))))

(define (execute uuid header content)
  (call-with-current-continuation
   (lambda (kappa)
     (with-restart
      'jupyter-error "report error to jupyter"
      (effector kappa uuid header) #f
      (lambda ()
	(fluid-let ((standard-error-hook error-hook))
	  (let ((expressions (get-expressions content))
		(environment (get-environment header)))
	    (pp (list "expressions" expressions))
	    (let ((ret (fold-right (evaluate environment) none expressions)))
	      (execute-result uuid header (caddr environment) ret)
	      (cons "ok" (caddr environment))))))))))

(define (execute-reply socket uuid header content)
  (pp "execute reply!")
  (let ((result (execute uuid header content)))
    (pp result)
    (reply socket uuid header "execute_reply"
	   `((status . ,(car result))
	     (execution_count . ,(cdr result))
	     (payload . ())
	     (user_expressions . ())))))

(define (execute-result uuid header id value)
  (let ((content `((data . ((text/plain . ,(write-to-string value))))
		   (metadata . ())
		   (execution_count . ,id))))
    (reply iopub-socket uuid header "execute_result" content)))

(define (error-result uuid header id report)
  (let ((content `((data . ((text/plain . ,report)))
		   (execution_count . ,id))))
    (pp "error-result!")
    (reply iopub-socket uuid header "error" content)))
