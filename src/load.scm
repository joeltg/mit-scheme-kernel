(define working-directory (working-directory-pathname))
(cd "/usr/local/share/jupyter/kernels/mit-scheme")

(load "import")

(import "json/json-decode" json-decode)
(import "kernel/kernel" listen)

(cd working-directory)

(define args (command-line))
(assert (> (length args) 0))

(define file (open-input-file (car args)))
(define text (read-string (char-set) file))
(define json (json-decode text))

(assert (and (vector? json)
	     (= 1 (vector-length json))))

(define (ref key)
  (cdr (assq key (vector-ref json 0))))
(define control-port     (ref 'control_port))
(define shell-port       (ref 'shell_port))
(define transport        (ref 'transport))
(define signature-scheme (ref 'signature_scheme))
(define stdin-port       (ref 'stdin_port))
(define hb-port          (ref 'hb_port))
(define ip               (ref 'ip))
(define iopub-port       (ref 'iopub_port))
(define key              (ref 'key))

(listen transport
	ip
	signature-scheme
	key
	control-port
	shell-port
	stdin-port
	hb-port
	iopub-port)
