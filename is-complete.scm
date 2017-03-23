(define ((complete-hook s) condition)
  (invoke-restart
   (find-restart 'jupyter-complete)
   (let ((name (condition-type/name (condition/type condition))))
     (cond ((string=? name "premature-eof")
	    `((status . "incomplete")
	      (indent . ,(make-string
			  (* 2 (abs (- (length (string-search-all "(" s))
				       (length (string-search-all ")" s)))))
			  #\space))))
	   ((string=? name "illegal-char")
	    '((status . "invalid")))
	   (else '((status . "invalid")))))))

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
	(let ((s (cdr (assq 'code content))))
	  (fluid-let ((standard-error-hook (complete-hook s)))
	    (let ((code (open-input-string s)))
	      (let iter ((exp (read code)))
		(if (eof-object? exp)
		    `((status . "complete"))
		    (iter (read code))))))))))))

(define (is-complete-reply socket uuid header content)
  (reply socket uuid header "is_complete_reply" (completion content)))

