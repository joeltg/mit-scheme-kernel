(define (continuation->reductions/frames continuation)
  (let loop ((frame (stack-frame/skip-non-subproblems
		     (continuation->stack-frame continuation)))
	     (reductions/frames '()))
    (if (and frame (not (stack-frame/repl-eval-boundary? frame)))
	(loop (stack-frame/next-subproblem frame)
	      (cons (cons frame (stack-frame/reductions frame))
		    reductions/frames))
	reductions/frames)))

(define reduction/expression car)
(define reduction/environment cadr)

(define (format-reduction reduction)
  (let ((expression
	 (format-expression (reduction/expression reduction)))
	(environment
	 (format-environment (reduction/environment reduction))))
    (splice expression environment)))

(define prefix "-- ")

(define (splice exp env)
  (string-append env "\n" exp))

;; this is the gnarliest shit you'll ever see in your life
(define ((split-fold s kl) p a)
  (cons p (cons (substring s (+ kl p) (car a)) (cdr a))))

(define (split s k)
  (let ((kl (string-length k))
	(sl (string-length s))
	(ps (string-search-all k s)))
    (cdr (fold-right (split-fold s kl)
		     (list sl)
		     (cons (- kl) ps)))))

(define (join l k)
  (if (null? l) ""
      (fold-left
       (lambda (s e) (string-append s k e))
       (car l)
       (cdr l))))

(define ((pre k) l)
  (string-append k l))

(define ((pda k) s)
  (join (map (pre k) (split s "\n")) "\n"))

(define (format-reductions reductions)
  (if (null? reductions)
      ""
      (let ((rs (map format-reduction reductions)))
	(join (cons (car rs) (map (pda prefix) (cdr rs))) "\n"))))

(define (format-frame frame)
  (with-values
    (lambda () (stack-frame/debugging-info frame))
    (lambda (exp env subexp)
      (string-append (format-environment env)
		     "\n"
		     (format-expression exp)))))

(define (format-reductions/frames reductions/frame)
  (let ((reductions (cdr reductions/frame))
	(frame (car reductions/frame)))
    (string-append (format-frame frame)
		   (format-reductions reductions))))

(define (format-stack continuation)
  (list->vector
   (map format-reductions/frames
	(continuation->reductions/frames continuation))))

(define (format-expression expression)
  (cond
   ((debugging-info/compiled-code? expression) "<compiled code>")
   ((not (debugging-info/undefined-expression? expression))
    (fluid-let ((*unparse-primitives-by-name?* #t))
      (let ((string (open-output-string)))
	(get-output-string string))))
   ((debugging-info/noise? expression)
    (write-to-string ((debugging-info/noise expression) #f)))
   (else "<undefined expression>")))

(define (format-environment environment)
  (let ((name (and (environment? environment)
		   (environment-procedure-name environment))))
    (cond
     ((string? name) name)
     ((interned-symbol? name) (symbol-name name))
     (else (write-to-string name)))))

(define (error-hook condition)
  (invoke-restart (find-restart 'jupyter-error)
		  (condition-type/name (condition/type condition))
		  (condition/report-string condition)
		  (format-stack (condition/continuation condition))))

(define ((error-effector kappa session uuid header) name report stack)
  (error-result session uuid header name report stack)
  (kappa
   `((status . "error")
     (execution_count . ,(session-count session)))))

(define (with-error kappa session uuid header thunk)
  (with-restart
   'jupyter-error "report error to jupyter"
   (error-effector kappa session uuid header) #f
   (lambda ()
     (fluid-let ((standard-error-hook error-hook))
       (thunk)))))

(define (colorize report)
  (string-append "\033[31m" report "\033[0m"))

(define (error-result session uuid header name report stack)
  (let ((content `((ename . ,name)
		   (evalue . ,report)
		   (traceback . #(,(colorize report)))
;		   (traceback . ,(vector-append stack (vector report)))
		   (execution_count . ,(session-count session))
		   (user_expressions))))
    (reply iopub-socket uuid header "error" content)))
