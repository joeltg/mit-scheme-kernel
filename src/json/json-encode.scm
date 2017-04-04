(define json-encode)

(let ()

  (define (json-atom l)
    (cons l (last-pair l)))

  (define (number->json number)
    (if (integer? number)
	(json-atom (string->list (number->string number)))
	(let ((r (abs number))
	      (s (< 0 number)))
	  (let ((l (string->list (number->string (exact->inexact r)))))
	    (let ((l (if (< 1 r) (cons #\0 l) l)))
	      (let ((l (if s (cons #\- l) l)))
		(cons l (last-pair l))))))))

  (define (escape-char char str)
    (cond
     ((char=? char #\\) (cons #\\ (cons #\\ str)))
     ((char=? char #\") (cons #\\ (cons #\" str)))
     ((char=? char #\newline) (cons #\\ (cons #\n str)))
     ((char=? char #\escape)
      (cons #\\ (cons #\u (cons #\0 (cons #\0 (cons #\1 (cons #\b str)))))))
     (else (cons char str))))

  (define (string->json s)
    (if (string-null? s)
	(json-atom (list #\" #\"))
	(let ((initial (list #\")))
	  (let ((l (fold-right escape-char initial (string->list s))))
	    (cons (cons #\" l) initial)))))

  (define (symbol->json symbol)
    (string->json (symbol->string symbol)))

  (define (boolean->json boolean)
    (json-atom (string->list (if boolean "true" "false"))))

  (define (null->json null)
    (json-atom (string->list "null")))

  (define (json-splice e a)
    (set-cdr! (cdr e) a)
    (cons #\, (car e)))

  (define (vector->json v)
    (if (= (vector-length v) 0)
	(json-atom (list #\[ #\]))
	(let ((initial (list #\])))
	  (cons 
	   (cons 
	    #\[ 
	    (cdr (fold-right json-splice initial (map json (vector->list v)))))
	   initial))))

  (define (pair->json p)
    (let ((key (symbol->json (car p)))
	  (value (json (cdr p)))
	  (pivot (list #\:)))
      (set-cdr! pivot (car value))
      (set-cdr! (cdr key) pivot)
      (cons (car key) (cdr value))))

  (define (alist->json a)
    (if (null? a)
	(list #\{ #\})
	(let ((initial (list #\})))
	  (cons
	   (cons
	    #\{
	    (cdr (fold-right json-splice initial (map pair->json a))))
	   initial))))
  
  (define (json-error object)
    (error "invalid json object" object))

  (define (json object)
    (cond
     ((null? object) (json-atom (string->list "{}")))
     ((eq? #!unspecific object) (null->json object))
     ((boolean? object) (boolean->json object))
     ((symbol? object) (symbol->json object))
     ((string? object) (string->json object))
     ((number? object) (number->json object))
     ((vector? object) (vector->json object))
     ((alist? object) (alist->json object))
     (else (json-error object))))

  (set! json-encode 
	(lambda (object)
	  (list->string (car (json object))))))
