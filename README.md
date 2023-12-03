# chex

**chex** lets you run a REPL in a separate Lisp instance and fully
communicate with it over streams.

It uses `uiop:launch-program` to asynchronously launch a program and get its
i/o over streams.

## Installation

**chex** is available on [Ultralisp](https://ultralisp.org/). 

Assuming you have added Ultralisp to your quicklisp, just run
```lisp
(ql:quickload :chex)
```
You may need to run `(ql:update-dist "ultralisp")` to get the latest version of **chex**.

or if Ultralisp is down you can use quicklisp's local-projects mechanism:
```bash
cd ~/quicklisp/local-projects
git clone https://github.com/oliverdelancey/chex.git
```

## Compatibility

**chex** has only been tested with SBCL, but *should* be either
implementation-independent or trivially easy to add compatibility. Hit up 
[Issues](https://github.com/oliverdelancey/chex/issues) if there's an 
implementation that's not working.

**chex** has been tested on Windows 11 and Linux (Fedora Workstation), so it should also
work on Windows 10 and other Linux distributions.

## Documentation

All functions/methods/classes have docstrings, so `describe` is your friend.

## Usage

See [start.lisp](start.lisp) for an example.

### Start a repl:
```lisp
(defparameter *repl* 
  (chex:start-repl 
    "sbcl" 
    (asdf:system-relative-pathname "chex" "child/chex-init.lisp")))
```
[chex-init.lisp](child/chex-init.lisp) redirects relevant streams in the child 
process to allow reading/writing to all [standard streams](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node183.html#SECTION002510000000000000000) 
through `*standard-output*` and `*standard-input*`. It also adds `:chex-child` 
to `*features*`.

It is provided as an option so you can supply your own file if needed, to do
whatever setup you need in the child process.

### Read the repl's output:
```lisp
(chex:read-strings-from-repl *repl*)
```
This will return 2 values: the output of `IO-STREAM`, the main i/o stream, and 
the output of `ERROR-STREAM`, the error output stream.

### Send a string to the repl:
```lisp
(chex:send-string-to-repl str *repl*)
``` 
This should input to any input stream in the child process, including the 
interactive debugger (`*debug-io*`) and queries (`*query-io*`), as long as they
have been properly redirected with `chex-init.lisp`.

### Close the repl:
```lisp
(chex:close-repl *repl*)
```
This should close the process and all related streams. Returns `T` if the close
was successful, `NIL` otherwise.
