;;;;
;;;; CHEX-INIT.LISP
;;;;
;;;; Set up client process to work with CHEX
;;;;


(setf *debug-io* (make-two-way-stream *standard-input* *standard-output*))
(setf *query-io* (make-two-way-stream *standard-input* *standard-output*))
;(setf *trace-output* *standard-output*)

(push :chex-child *features*)
