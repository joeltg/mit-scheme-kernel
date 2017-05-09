(cd "/usr/local/share/jupyter/kernels/mit-scheme")

(load "json/json-encode")
(load "json/json-decode")
(load "zmq/zmq")

(define shared-env (make-top-level-environment))
(load "shared" shared-env)
(define env (the-environment))
(for-each
  (lambda (binding) (apply environment-define env binding))
  (environment-bindings shared-env))
(environment-define shared-env 'shared-env shared-env)

(load "kernel/utils")
(load "kernel/info")
(load "kernel/error")
(load "kernel/stdio")
(load "kernel/shutdown")
(load "kernel/session")
(load "kernel/complete")
(load "kernel/execute")
(load "kernel/comm/comm")
(load "kernel/comm/version")
(load "kernel/comm/widget/widget")
(load "kernel/comm/widget/backbone")
(load "kernel/comm/widget/custom")
(load "kernel/kernel")

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
