(import "../../shared"
  print
  session-comms
  comm-target
  comm-id)
(import "../utils" asss)

(define comm-targets '())
(define (add-comm-target! comm-target)
  (set! comm-targets (cons comm-target comm-targets)))
(define make-comm-target list)
(define comm-target-name first)
(define comm-target-open second)
(define comm-target-handler third)

(define (get-comm-target target-name)
  (let ((target (asss target-name comm-targets)))
    (or target (error "invalid comm target" target-name))))

(define comm-ref (association-procedure string=? comm-id))

(define (comm-info-request session content reply pub . env)
  (pub "comm_info_reply" '((status . "ok") (comms))))

(define (comm-open session content reply pub . env)
  (let ((comm-id (cdr (assq 'comm_id content)))
	      (data (cdr (assq 'data content)))
	      (target-name (cdr (assq 'target_name content))))
    (let ((comm-target (get-comm-target target-name)))
      ((comm-target-open comm-target) session pub comm-id data))))

(define (comm-msg session content reply pub . env)
  (pub "status" '((execution_state . "busy")))
  (let ((id (cdr (assq 'comm_id content)))
	      (data (cdr (assq 'data content))))
    (let ((target-name (comm-target (comm-ref id (session-comms session)))))
      (let ((comm-target (get-comm-target target-name)))
        ((comm-target-handler comm-target) session pub id data))))
  (pub "status" '((execution_state . "idle"))))

(export
  make-comm-target
  comm-info-request
  comm-open comm-msg
  add-comm-target!)