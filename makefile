all: build

install: install-zmq install-kernel

install-zmq:
	echo '(install-shim "$(DESTDIR)" "zmq")' | mit-scheme --batch-mode

install-kernel: make-directory copy-files

make-directory:
	mkdir -p /usr/local/share/jupyter/kernels/mit-scheme

copy-files:
	cp -r src/* /usr/local/share/jupyter/kernels/mit-scheme/.

clean:
	rm zmq-const* zmq-types* zmq-shim*

build: zmq-shim.so zmq-types.bin zmq-const.bin

zmq-shim.so: zmq-shim.o
	echo "(link-shim)" | mit-scheme --batch-mode -- -o $@ $^ -L/usr/local/lib -lzmq

zmq-shim.o: zmq-shim.c
	echo '(compile-shim)' | mit-scheme --batch-mode -- -I/usr/local/include -c $<

zmq-shim.c zmq-const.c zmq-types.bin: zmq.cdecl
	echo '(generate-shim "zmq" "#include <zmq.h>")' | mit-scheme --batch-mode

zmq-const.bin: zmq-const.scm
	echo '(sf "zmq-const")' | mit-scheme --batch-mode

zmq-const.scm: zmq-const
	./zmq-const

zmq-const: zmq-const.o
	$(CC) -o $@ $^ $(LDFLAGS) -L/usr/local/lib -lzmq

zmq-const.o: zmq-const.c
	$(CC) -I/usr/local/include $(CFLAGS) -o $@ -c $<
