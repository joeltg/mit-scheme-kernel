;; a session has an id (from client), an execution counter,
;; and an environment
(import "../shared"
  initialize-session
  session-id
  session-env
  session-count
  session-stdio
  set-session-pub!
  set-session-count!
  set-session-stdio!)
(import "stdio" make-stdio)

(define source-directory 
  (pathname-simplify
    (merge-pathnames
      "../"
      (working-directory-pathname))))

(define runtime
  (pathname-simplify
    (merge-pathnames
      "runtime/runtime.scm"
      source-directory)))

(define (prepare-session! session pub)
  (set-session-pub! session pub)
  (let ((env (session-env session)))
    (environment-define env 'session session)))

(define session-ref (association-procedure string=? session-id))

(define (initialize-env! session)
  (let ((env (session-env session)))
    (environment-define env '*source-directory* source-directory)
    (environment-define env '*session* session)
    (load runtime env)))

(define (make-session identity id)
  (let ((session (initialize-session identity id)))
    (set-session-stdio! session (make-stdio #f))
    (set-port/state! (session-stdio session) session)
    (initialize-env! session)
    session))

(define (session-count! session)
  (set-session-count! session (+ 1 (session-count session))))

(export
  prepare-session!
  session-ref
  make-session
  session-count!)