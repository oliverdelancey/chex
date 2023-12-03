;;;;
;;;; CHEX : EXecute commands in CHild process
;;;;


(in-package #:chex)


(defclass repl ()
  ((io-stream :initarg :io-stream
              :accessor io-stream)
   (error-stream :initarg :error-stream
                 :accessor error-stream)
   (process :initarg :process
            :accessor process))
  (:documentation "repl convenience class"))

(defun start-repl (program init-file)
  "create a REPL instance running PROGRAM with loaded INIT-FILE"
  (let ((process (uiop:launch-program 
                   (format nil "~a --load ~a" program init-file)
                   :input :stream
                   :output :stream
                   :error-output :stream)))
    (when process
      (make-instance 'repl
                     :io-stream (make-two-way-stream
                                  (uiop:process-info-output process)
                                  (uiop:process-info-input process))
                     :error-stream (uiop:process-info-error-output process)
                     :process process))))

(defmethod exit-code ((repl repl))
  "get the exit code of the process running under REPL"
  (slot-value (process repl) 'uiop/launch-program::exit-code))

(defmethod open-stream-p ((repl repl))
  "true if IO-STREAM and ERROR-STREAM of REPL are open, false otherwise"
  (and (open-stream-p (io-stream repl))
       (open-stream-p (error-stream repl))))

(defmethod running-p ((repl repl))
  "true if EXIT-CODE of REPL is NIL, false otherwise

  EXIT-CODE is only assigned an integer value once the process under REPL is
  closed."
  (null (exit-code repl)))

(defmethod viable-p ((repl repl))
  "true if EXIT-CODE is non-NIL and IO-STREAM and ERROR-STREAM are open"
  (and (open-stream-p repl)
       (running-p repl)))


(defun send-to-stream (string stream)
  "send STRING to STREAM with standard i/o syntax"
  (with-standard-io-syntax
    (format stream "~a~%" string))
  (finish-output stream))

(defmethod send-string-to-repl ((expr string) (repl repl))
  "send EXPR to REPL"
  (send-to-stream expr (io-stream repl)))

(defun read-stream-until-empty-to-string (stream)
  "read STREAM until until there are no characters left"
  (coerce (loop for c = (read-char-no-hang stream)
                while c
                collect c)
          'string))

(defmethod read-strings-from-repl ((repl repl))
  "read contents of IO-STREAM and ERROR-STREAM from REPL into strings"
  (values (read-stream-until-empty-to-string (io-stream repl))
          (read-stream-until-empty-to-string (error-stream repl))))


(defmethod close-repl ((repl repl))
  "close IO-STREAM and ERROR-STREAM and stop the PROCESS running under REPL

  Return true on success, false otherwise."
  (when (viable-p repl)
    (send-string-to-repl "(quit)" repl)
    (uiop:close-streams (process repl))
    (close (io-stream repl))  ; possibly redundant
    (close (error-stream repl))  ; possibly redundant
    #+win32  ; UIOP:TERMINATE-PROCESS doesn't work well on Windows
    (uiop:run-program (list "taskkill" "/F" "/PID" 
                            (write-to-string 
                              (uiop:process-info-pid
                                (process repl)))))
    #-win32
    (uiop:terminate-process (process repl))
    (uiop:wait-process (process repl)))
  (not (uiop:process-alive-p (process repl))))
