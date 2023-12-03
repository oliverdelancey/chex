(ql:quickload :chex)

(defparameter *repl* 
  (chex:start-repl 
    "sbcl" 
    (asdf:system-relative-pathname "chex" "child/chex-init.lisp")))

(defun rs () (chex:read-strings-from-repl *repl*))

(defun ss (str) (chex:send-string-to-repl str *repl*))

(defun cls () (chex:close-repl *repl*))

