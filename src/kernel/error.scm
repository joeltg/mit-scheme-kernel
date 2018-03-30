(define (error-hook condition)
  (invoke-restart (find-restart 'jupyter-error)
		  (condition-type/name (condition/type condition))
		  (condition/report-string condition)))

(define ((effector kappa session) name report)
  (error-result session name report)
  (kappa "error"))

(define (error-result session name report )
  (let ((content `((ename . ,name)
		   (evalue . ,report)
		   (traceback . #(,(colorize report)))
		   (execution_count . ,(session-count session))
		   (user_expressions))))
    ((session-pub session) "error" content)))

(define has-fluid (environment-bound? (the-environment) 'let-fluid))

(define (with-error session thunk)
  (call-with-current-continuation
   (lambda (kappa)
     (with-restart
      'jupyter-error "report error to jupyter client"
      (effector kappa session)
      #f
      (lambda ()
        (if has-fluid
          (let-fluid standard-error-hook error-hook 
            (lambda ()
              (thunk)
              "ok"))
          (fluid-let ((standard-error-hook error-hook))
            (thunk)
            "ok")))))))