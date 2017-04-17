;; a session has a uuid (from client), an execution counter,
;; and an environment

(define (prepare-session! session pub)
  (set-session-pub! session pub)
  (let ((env (session-env session)))
    (environment-define env 'session session)))

(define (default-pub . args)
  (apply print "default pub!" args))

(define-structure
  (session (constructor initialize-session (uuid)))
  (uuid)
  (pub default-pub)
  (count 0)
  (stdio (make-stdio #f))
  (env (make-top-level-environment))
  (comms '())
  (widgets '()))

(define session-ref (association-procedure string=? session-uuid))

(define (initialize-env! env)
  (environment-define env 'session-add-comm! session-add-comm!)
  (environment-define env 'session-add-widget! session-add-widget!)
  (environment-define env 'open-comm open-comm)
  (environment-define env 'send-comm-msg send-comm-msg)
  (environment-define env 'session-pub session-pub)
  (environment-define env 'comm-session comm-session)
  (environment-define env 'comm-id comm-id)
  (load "runtime.scm" env))

(define (make-session uuid)
  (let ((session (initialize-session uuid)))
    (set-port/state! (session-stdio session) session)
    (initialize-env! (session-env session))
    session))

(define (session-count! session)
  (set-session-count! session (+ 1 (session-count session))))

(define (session-add-comm! session comm)
  (set-session-comms! session (cons comm (session-comms session))))

(define (session-add-widget! session widget)
  (set-session-widgets! session (cons widget (session-comms session))))
