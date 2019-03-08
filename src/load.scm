(define working-directory (working-directory-pathname))
(cd "/usr/local/share/jupyter/kernels/mit-scheme")

(load "import")

(import-from "json/json-decode" json-decode)
(import-from "kernel/kernel" listen)

(cd working-directory)

(define args (command-line))
(assert (> (length args) 0))


(define file (open-input-file (last args)))
(define text (read-string (char-set) file))
(define json (json-decode text))
(assert
  (and (vector? json)
    (= 1 (vector-length json))))

(define (ref key)
  (cdr (assq key (vector-ref json 0))))

(listen (ref 'transport)
  (ref 'ip)
  (ref 'signature_scheme)
  (ref 'key)
  (ref 'control_port)
  (ref 'shell_port)
  (ref 'stdin_port)
  (ref 'hb_port)
  (ref 'iopub_port))
