;; a session has an id (from client), an execution counter,
;; and an environment

(define runtime (merge-pathnames source-pathname "runtime.scm"))

(define (prepare-session! session pub)
  (set-session-pub! session pub)
  (let ((env (session-env session)))
    (environment-define env 'session session)))

(define session-ref (association-procedure string=? session-id))

(define (initialize-env! env)
  (load runtime env))

(define (make-session identity id)
  (let ((session (initialize-session identity id)))
    (set-session-stdio! session (make-stdio #f))
    (set-port/state! (session-stdio session) session)
    (initialize-env! (session-env session))
    session))

(define (session-count! session)
  (set-session-count! session (+ 1 (session-count session))))
