;; a session has a uuid (from client), an execution counter,
;; and an environment

(define (prepare-env! pub session)
  (define env (session/env session))
  (let ((env (session/env session)))
    (environment-define env '*pub* pub)
    (environment-define env '*session* session)
    (load "runtime.scm" env)))

(define (make-session uuid pub)
  (let ((stdio (make-stdio pub))
	(env (make-top-level-environment)))
    (environment-define env '*session*/add-comm! session/add-comm!)
    (environment-define env '*make-msg-id* make-msg-id)
    (environment-define env '*make-comm* make-comm)
    (list uuid 0 env stdio '() '())))

(define session/uuid  first)
(define session/count second)
(define session/env   third)
(define session/stdio fourth)
(define session/comms fifth)
(define session/widgets sixth)

(define (session/count! session)
  (set-car! (cdr session) (+ 1 (cadr session))))

(define (session/add-comm! session comm)
  (set-car! (cddddr session)
	    (cons comm (fifth session))))

(define (session/add-widget! session widget)
  (set-car! (cdr (cddddr session))
	    (cons widget (sixth session))))
