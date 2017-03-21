(define (complete-hook condition)
  (invoke-restart
   (find-restart 'jupyter-complete)
   (let ((name (condition-type/name (condition/type condition))))
     (cond ((string=? name "premature-eof") "incomplete")
	   ((string=? name "illegal-char") "invalid")
	   (else "invalid")))))

(define ((complete-effector kappa) status)
  (kappa status))

(define (is-complete-request socket uuid json)
  (let ((header (get-header json))
	(content (get-content json)))
    (pub uuid header "busy")
    (is-complete-reply socket uuid header content)
    (pub uuid header "idle")))

(define (completion content)
  (call-with-current-continuation
   (lambda (kappa)
     (with-restart
      'jupyter-complete "check code completion"
      (complete-effector kappa) #f
      (lambda ()
        (let ((code (open-input-string (cdr (assq 'code content)))))
	  (let iter ((exp (read code)))
	    (if (eof-object? exp)
		"complete"
		(iter (read code))))))))))

(define (is-complete-reply socket uuid header content)
  (reply
   socket uuid header
   `((status . ,(completion content)))))

