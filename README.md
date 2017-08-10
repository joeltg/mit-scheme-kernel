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

macOS *should* work but is sometimes funky; if you get stuck on `ld: file not found: scheme` inside `(link-shim)`, copy the printed `gcc...` command and run it after replacing `scheme` with the absolute path to the scheme executable (usually `/usr/local/bin/mit-scheme` or something). I don't actually know how makefiles work and made all of this by pattern-matching off previous work.

### Future
If anyone would like to write a decent interactive debugger like Edwin's that'd be dope.
