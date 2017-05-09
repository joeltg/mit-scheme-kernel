(define inspectors '())
(define make-inspector cons)
(define inspector-type car)
(define inspector-effector cdr)

(define (attach-inspector! predicate effector)
  (set! inspectors
    (cons
      (make-inspector type effector)
      inspectors)))

(define ((inspect-find inspector) object)
  ((inspector-effector inspector) object))

(define (inspect object . args)
  (let ((inspector (find (inspect-find object) inspectors)))
    (if inspector
      (apply (inspector-effector inspector) object args)
      (error "invalid inspect application"))))

