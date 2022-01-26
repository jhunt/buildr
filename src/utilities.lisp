;;;
;;; src/utilities.lisp
;;; ©2022 James Hunt
;;;
;;; This file contains helpful utilies that make
;;; the rest of the BUILDR implementation easier.
;;; Lots of these, like KEYWORDF and INTERNF, aid
;;; our meta-programming efforts.

(in-package #:buildr)

(defun keywordf (&rest args)
  "INTERNs an arbitrary list of string or string-like values into the KEYWORD package"
  (intern
    (format nil "~:@(~{~A~}~)" args)
    'keyword))

(defun internf (&rest args)
  "INTERNs an arbitrary list of string or string-like values into the current package"
  (intern
    (format nil "~:@(~{~A~}~)" args)))

(defmacro defsimpleclass (name fields)
  "Defines a simple class, one with all public fields and without specialized accessors, initargs, or initforms"
  `(progn
     (defclass ,name ()
       ,(mapcar #'(lambda (f)
                    `(,f :initarg ,(keywordf f)
                         :accessor ,(internf name "-" f)))
                fields))
     (defmacro ,(internf "make-" name) (&rest args)
       `(make-instance ',',name ,@args))))

(defun read-file (path)
  "Reads PATH as a single Lisp form"
  (with-open-file (in path)
    (read in)))

(defun just (fn)
  "Wraps FN in a LAMBDA that ignores all of its arguments"
  #'(lambda (&rest ignored)
      (declare (ignore ignored))
      (apply fn nil)))

(defun gensyms (for)
  "Returns a list of gensyms, one for each element in FOR"
  (mapcar (just #'gensym) for))

(defmacro with-gensyms (vars &body body)
  "Wraps a VARS (a list of symbols) in gensyms and then expands BODY within that new lexical context"
  `(let (,@(mapcar #'(lambda (v)
                       `(,v (gensym)))
                   vars))
     ,@body))
(defun f+ (f g)
  "Composes F and G and returns a lambda that calls (F (G V)) for a given argument V"
  #'(lambda (v) (funcall f (funcall g v))))

(defun f* (fn &rest fixed-args)
  "Curries FN with the given FIXED-ARGS, returning a LAMBDA that can accept additional arguments"
  #'(lambda (&rest variable-args)
      (apply fn (append fixed-args
                        variable-args))))


(defun let+-decls (decls body)
  "A helper function for the LET+ macro, to generate the appropriate nested LET forms"
  (cond (decls
          `(let ((,(caar decls)
                  ,(cadar decls)))
             (when ,(caar decls)
               ,(let+-decls (cdr decls) body))))
        (t `(progn ,@body))))

(defmacro let+ (decls &body body)
  "Similar to a LET form except that the value of each bound declaration in DECLS is checked for NIL-ness along the way; the BODY is evaluated in the new lexical environment only if all bindings are non-NIL"
  (let+-decls decls body))

(defun track+/rest (key hash value keys)
  (let ((h (gethash key hash)))
    (cond ((null keys)
           (setf (gethash key hash) value))
          ((null h)
           (let ((h (make-hash-table :test #'equal)))
             (setf (gethash key hash) h)
             (track+/rest (car keys) h value (cdr keys)))))))

(defun track+ (keys hash value)
  "Set multiple levels of KEYS in HASH to VALUE"
  (if keys
    (track* (car keys) hash value (cdr keys))))
