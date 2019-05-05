SCHEME=/usr/local/bin/mit-scheme
AUXDIR=/usr/local/lib/mit-scheme-x86-64
DESTDIR=/usr/local/share/jupyter/kernels/mit-scheme

all: build

install: install-zmq install-kernel

install-zmq:
	install -m 644 zmq-types.bin $(AUXDIR)
	install -m 644 zmq-const.bin $(AUXDIR)
	install -m 644 zmq-const.scm $(AUXDIR)
	install -m 644 zmq-shim.so $(AUXDIR)

install-kernel:
	rm -rf $(DESTDIR)
	cp -r src $(DESTDIR)

clean:
	rm zmq-const* zmq-types* zmq-shim*

build: zmq-shim.so zmq-types.bin zmq-const.bin

zmq-shim.so: zmq-shim.o
	$(CC) -shared -fPIC -o $@ $^ `pkg-config --libs libzmq`

zmq-shim.o: zmq-shim.c
	$(CC) -I$(AUXDIR) -Wall -fPIC `pkg-config --cflags libzmq` -o $@ -c $<

zmq-shim.c zmq-const.c zmq-types.bin: zmq.cdecl
	echo '(generate-shim "zmq" "#include <zmq.h>")' | $(SCHEME) --batch-mode

zmq-const.bin: zmq-const.scm
	echo '(sf "zmq-const")' | $(SCHEME) --batch-mode

zmq-const.scm: zmq-const
	./zmq-const

zmq-const: zmq-const.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS) `pkg-config --libs libzmq`

zmq-const.o: zmq-const.c
	$(CC) `pkg-config --cflags libzmq` $(CFLAGS) -o $@ -c $<
