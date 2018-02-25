(load-option '*parser)

(define (json-vector-list v)
  (vector (vector->list v)))

(define json-object-pair
  (*parser
    (transform
      (lambda (v)
        (vector (cons (string->symbol (vector-ref v 0)) (vector-ref v 1))))
      (seq
        json-string
        (noise
          (seq
            (* (char-set char-set:whitespace)) 
            ":"
            (* (char-set char-set:whitespace)))) 
        json-value))))

(define json-object
  (*parser
    (transform
      (lambda (v)
        (vector (vector->list v)))
      (seq
       "{"
        (noise (* (char-set char-set:whitespace)))
        (? (seq (*
		 (seq
		   (noise (* (char-set char-set:whitespace)))
		   json-object-pair
		   (noise
		     (seq
		       (* (char-set char-set:whitespace))
		       ","
		       (* (char-set char-set:whitespace))))))
		json-object-pair
		(noise (* (char-set char-set:whitespace)))))
        "}"))))

(define json-array
  (*parser
    (transform
      vector
      (seq
        "["
        (? (seq (* (seq json-value ",")) json-value))
        "]"))))

(define json-value
  (*parser
    (seq
      (noise (* (char-set char-set:whitespace)))
      (alt
        json-string
        json-number
        json-object
        json-array
        (transform
          (lambda (v)
            (let ((const (vector-ref v 0)))
              (cond
                ((string=? "true" const) (vector #t))
                ((string=? "false" const) (vector #f))
                ((string=? "null" const) (vector #!unspecific)))))
          (match (alt "true" "false" "null"))))
      (noise (* (char-set char-set:whitespace))))))

(define char-set:hex
  (char-set-union
    char-set:numeric
    (string->char-set "abcdefABCDEF")))

(define char-set:unicode
  (char-set-invert
    (scalar-values->char-set '((#xD800 . #xDFFF) #xFFFE #xFFFF))))

(define char-set:json
  (char-set-difference
    char-set:unicode
    (string->char-set "\"\\")))

(define json-string-hex-digit
  (*parser
    (transform
      json-char-map
      (match (char-set char-set:hex)))))

(define (json-digit-map v)
  (integer->char (string->number (list->string (vector->list digits)) 16) 0))

(define json-string-unicode 
  (*parser
    (transform
      json-digit-map
      (seq
        "u"
        json-string-hex-digit
        json-string-hex-digit
        json-string-hex-digit
        json-string-hex-digit))))

(define (json-char-map v)
  (vector-map name->char v))

(define (json-escape char)
  (cond
    ((char=? #\b char) #\backspace)
    ((char=? #\n char) #\newline)
    ((char=? #\f char) #\page)
    ((char=? #\t char) #\tab)
    ((char=? #\r char) #\return)
    (else char)))

(define (json-escape-map v)
  (vector-map json-escape (json-char-map v)))

(define json-string-char
  (*parser
    (alt
      (seq
        "\\"
        (alt
          json-string-unicode
          (transform
            json-escape-map
            (match (char-set (string->char-set "bnftr/\"\\"))))))
      (transform
        json-char-map
        (match (char-set char-set:json))))))

(define json-string
  (*parser
    (transform
      (lambda (v)
        (vector (list->string (vector->list v))))
      (seq "\"" (* json-string-char) "\""))))

(define json-number
  (*parser
    (transform
      (lambda (v)
        (vector (string->number (list->string (vector->list (json-char-map v))))))
      (seq
        (? (match "-"))
        (alt
          "0"
          (seq
            (match (char-set (char-set-difference char-set:numeric (char-set #\0))))
            (* (match (char-set char-set:numeric)))))
        (? 
          (seq
            (match ".")
            (+ (match (char-set char-set:numeric)))))
        (?
          (seq
            (match (char-ci #\e))
            (? (match (alt "+" "-")))
            (+ (match (char-set char-set:numeric)))))))))

(define (json-decode json-string)
  (json-value (string->parser-buffer json-string)))

(export json-decode)