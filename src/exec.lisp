;;;
;;; src/exec.lisp
;;; ©2022 James Hunt
;;;
;;; This file provides some utility functions
;;; and macros to make life easier when dealing
;;; with outside programs and their execution
;;; via UNIX process semantics.
;;;
;;; While it relies heavily on uiop:run-program,
;;; and some potentially non-portable SBCL calls
;;; for influencing environments, it should be
;;; fairly portable, even if it needs more
;;; porting.
;;;
;;; There are essentially two ways to invoke an
;;; external program (both of them synchronous):
;;;
;;;   (run '("ls" "-la" "/tmp"))
;;;
;;; and:
;;;
;;;   (system '("ls" "-la" "/tmp"))
;;;
;;; RUN is for situations where you want to print
;;; the results of the program's execution to the
;;; standard output stream.
;;;
;;; SYSTEM is for when you want more control over
;;; what happens with the programs output and exit
;;; code--they are both returned to you.
;;;
;;; There is also a macro, WITH-ENV, that allows
;;; you to temporarily override UNIX environment
;;; variables for the duration of the body forms.
;;; This can be useful for programs that rely on
;;; environment variables, like how git-clone uses
;;; $GIT_SSH_COMMAND when cloning SSH-able repos.
;;;
;;; It works like this:
;;;
;;;   (with-env ((an-env-var "VALUE")
;;;              (other-var  42))
;;;     (run '("/bin/sh" "-c" "set | grep VAR")))
;;;
;;; WITH-ENV resembles a LET form; this is no
;;; accident.  The variable slot in each binding
;;; will be turned into a suitable UNIX env var
;;; name (upcased, with underscores instead of
;;; hyphens) using ENV-NAME.  Each value slot will
;;; be stringified, by way of ENV-VALUE.
;;;
;;; In the above example, these two environment
;;; variables will be set for the duration of the
;;; /bin/sh execution:
;;;
;;;    AN_ENV_VAR="VALUE"
;;;    OTHER_VAR="42"
;;;

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
  (multiple-value-bind
    (stdout stderr exit)
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
  (multiple-value-bind
    (stdout exit)
    (system command)

    (format t "------------------------~%")
    (format t "~A~%" stdout)
    (format t "--------~%")
    (format t "exited ~D~%" exit)))
