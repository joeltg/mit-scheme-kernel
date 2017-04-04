(cd "/usr/local/share/jupyter/kernels/mit-scheme")

(load "json/json-encode")
(load "json/json-decode")
(load "zmq/zmq")

(load "kernel/utils")
(load "kernel/kernel-info")
(load "kernel/error")
(load "kernel/stdio")
(load "kernel/is-complete")
(load "kernel/execute")
(load "kernel/kernel")

(poll)
