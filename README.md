# Selebot

## Prerequisite

* Firstlly, install the Chez Scheme: https://github.com/cisco/ChezScheme

* All the libraries Selebot depends on can be found at https://github.com/Saigut/chezscheme-libs. You can reference to that repository and set up the libraries.

* Compile C code to shared library.

```
gcc -fPIC -shared -o csocket.so ./csocket.c
```

## Run

```
scheme ./server.ss
```
