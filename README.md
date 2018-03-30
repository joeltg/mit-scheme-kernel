# mit-scheme-kernel
Jupyter Kernel for MIT Scheme

![](demo.png)

## Installation

### Docker
[Kevin Kwok](https://github.com/antimatter15) published a [docker image](https://hub.docker.com/r/kkwok/jupyter-mit-scheme/) that does all the things:
```
docker run -it --rm -p 8888:8888 kkwok/jupyter-mit-scheme
```

### Source

First get MIT Scheme 9.2+
```
$ wget http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/9.2/mit-scheme-9.2-x86-64.tar.gz
$ tar xvf mit-scheme-9.2-x86-64.tar.gz
$ cd mit-scheme-9.2/src/
$ ./configure
$ make compile-microcode
$ sudo make install
```
Then get ZeroMQ 4.2.1+
```
$ wget https://github.com/zeromq/libzmq/releases/download/v4.2.1/zeromq-4.2.1.tar.gz
$ tar xvf zeromq-4.2.1.tar.gz
$ cd zeromq-4.2.1/
$ ./configure
$ make
$ sudo make install
```
And finally 
```
$ git clone https://github.com/joeltg/mit-scheme-kernel
$ cd mit-scheme-kernel
$ export LD_LIBRARY_PATH=/usr/local/lib
$ make
$ sudo make install
$ jupyter console --kernel mit-scheme
Jupyter console 5.1.0

MIT Scheme Kernel


In [1]: (fold-left cons '() (iota 4))
Out[1]: ((((() . 0) . 1) . 2) . 3)
```

### macOS Compatibility
On macOS/OS X, installation might fail with `ld: file not found: scheme` inside `(link-shim)`. If this happens, you'll have to find and edit your system's `shim-config.scm` to make sure the `-bundle-loader` flag of `LINK-SHIM` points to your Scheme executable.

If you built and installed the [portable C package](https://www.gnu.org/software/mit-scheme/liarc-build.html), `shim-config.scm` is probably located in `/usr/local/lib/mit-scheme-c/`; if you build and installed the [normal unix binary](https://www.gnu.org/software/mit-scheme/documentation/mit-scheme-user/Unix-Installation.html), then it's probably located in `/usr/local/lib/mit-scheme-x86-64/`.

Either way, you have to change `-bundle_loader scheme` to `-bundle_loader /usr/local/bin/scheme`. Your mileage may vary. Computers are weird.
