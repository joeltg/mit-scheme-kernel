(import-from "comm" make-comm-target add-comm-target!)
(import-from "../../shared" print make-comm send-comm-msg)

(define comm-version "~2.1.4")
(define comm-version-name "jupyter.widget.version")
(define (comm-version-handler session pub id data)
  (if (cdr (assq 'validated data))
      (print "widget comms validated")
      (warn "invalid jupter widget version")))

(define (comm-version-open session pub id data)
  (let ((comm (make-comm session comm-version-name id)))
    (send-comm-msg comm `((version . ,comm-version)))))

(define comm-version-target 
    (make-comm-target comm-version-name comm-version-open comm-version-handler))
(add-comm-target! comm-version-target)
