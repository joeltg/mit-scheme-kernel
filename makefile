# SCMUTILS = /usr/local/scmutils/mit-scheme
# SCHEME = $(SCMUTILS)/bin/scheme --library $(SCMUTILS)/lib/
# SCHEME = scheme --library $(SCMUTILS)/lib/
# SCHEME = $(SCMUTILS)/bin/scheme
SCHEME = scheme

all: build

install: install-zmq install-kernel

install-zmq:
	echo '(install-shim "$(DESTDIR)" "zmq")' | $(SCHEME) --batch-mode

install-kernel: make-directory copy-files

make-directory:
	mkdir -p /usr/local/share/jupyter/kernels/mit-scheme

copy-files:
	cp -r src/* /usr/local/share/jupyter/kernels/mit-scheme/.

clean:
	rm zmq-const* zmq-types* zmq-shim*

build: zmq-shim.so zmq-types.bin zmq-const.bin

zmq-shim.so: zmq-shim.o
	echo "(link-shim)" | $(SCHEME) --batch-mode -- -o $@ $^ -L/usr/local/lib -lzmq

zmq-shim.o: zmq-shim.c
	echo '(compile-shim)' | $(SCHEME) --batch-mode -- -I/usr/local/include -c $<

zmq-shim.c zmq-const.c zmq-types.bin: zmq.cdecl
	echo '(generate-shim "zmq" "#include <zmq.h>")' | $(SCHEME) --batch-mode

zmq-const.bin: zmq-const.scm
	echo '(sf "zmq-const")' | $(SCHEME) --batch-mode

zmq-const.scm: zmq-const
	./zmq-const

zmq-const: zmq-const.o
	$(CC) -o $@ $^ $(LDFLAGS) -L/usr/local/lib -lzmq

zmq-const.o: zmq-const.c
	$(CC) -I/usr/local/include $(CFLAGS) -o $@ -c $<
