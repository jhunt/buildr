(in-package #:buildr)

(defun env-name (sym)
  "Converts the symbol SYM into a string suitable for use as a UNIX environment variable name"
  (substitute #\_ #\- (format nil "~Ae" sym)))

(defun env-val (thing)
  "Converts THING into a string suitable for use as a UNIX environment variable value"
  (format nil "~A" thing))

(defmacro with-env (decls &body body)
  "Temporarily overrides the current UNIX environment using DECLS, a list of (name value) sub-lists. NAME must be a symbol (it will be run through ENV-NAME); VALUE can be anything (it will be run through ENV-VALUE)."
  (let* ((syms (gensyms decls))
         (vars (mapcar (f+ #'env-name #'car) decls))
         (vals (mapcar (f+ #'env-val #'cadr) decls)))
    `(let ,(mapcar #'(lambda (sym var)
                       `(,sym (uiop:getenv ,var)))
                   syms vars)
       (unwind-protect
         (progn
           ,@(mapcar #'(lambda (var val)
                         `(setf (uiop:getenv ,var) ,val))
                     vars vals)
           ,@body)
         (progn
           ,@(mapcar #'(lambda (var sym)
                         `(setf (uiop:getenv ,var) ,sym))
                     vars syms))))))

(defun system (command)
  "Executes COMMAND in a new process, returning the standard output and the exit code as VALUES"
  (multiple-value-bind (stdout stderr exit)
    (uiop:run-program command
                      :input nil
                      :output '(:string :stripped t)
                      :error-output :output
                      :ignore-error-status t)
    (declare (ignore stderr))
    (values stdout exit)))

(defun run (command)
  "Runs COMMAND using SYSTEM, and prints standard output and exit code to the default output stream"
  (format t "~&running ~S~%" command)
  (multiple-value-bind (stdout exit)
    (system command)

    (format t "------------------------~%")
    (format t "~A~%" stdout)
    (format t "--------~%")
    (format t "exited ~D~%" exit)))
