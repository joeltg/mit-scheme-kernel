(define (error-hook condition)
  (invoke-restart (find-restart 'jupyter-error)
		  (condition-type/name (condition/type condition))
		  (condition/report-string condition)))

(define ((effector kappa session reply pub) name report )
  (error-result session pub name report )
  (kappa "error"))

(define (error-result session pub name report )
  (let ((content `((ename . ,name)
		   (evalue . ,report)
		   (traceback . #(,(colorize report)))
		   (execution_count . ,(session-count session))
		   (user_expressions))))
    (pub "error" content)))

(define (with-error session reply pub thunk)
  (call-with-current-continuation
   (lambda (kappa)
     (with-restart
      'jupyter-error "report error to jupyter client"
      (effector kappa session reply pub)
      #f
      (lambda ()
	(fluid-let ((standard-error-hook error-hook))
	  (thunk)
	  "ok"))))))

