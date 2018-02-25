(define kernel-info
  '((protocol_version . "5.1.0")
    (implementation . "mit-scheme-kernel")
    (implementation_version . "0.0.2")
    (language_info . ((name . "MIT Scheme")
		      (version . "9.2.1")
                      (mimetype . "application/x-scheme")
		      (file_extension . ".scm")
		      (pygments_lexer . "scheme")
		      (codemirror_mode . "scheme")))
    (banner . "MIT Scheme Kernel")
    (help_links . #(((text . "GitHub")
		     (url . "https://github.com/joeltg/mit-scheme-kernel"))))))


(define (kernel-info-request session content reply pub . env)
  (pub "status" '((execution_state . "busy")))
  (kernel-info-reply reply)
  (pub "status" '((execution_state . "idle"))))

(define (kernel-info-reply reply)
  (reply "kernel_info_reply" kernel-info))

(export kernel-info-request)