;;;;
;;;; PACKAGE.LISP
;;;;


(defpackage #:chex
  (:use #:cl)
  (:export #:start-repl
           #:close-repl
           #:send-string-to-repl
           #:read-strings-from-repl))

(in-package #:chex)
