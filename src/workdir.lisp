(in-package #:buildr)

(defparameter *workdir-root* "/tmp/buildr/")

(defun create-workdir ()
  "Provision a new temporary working directory, under *WORKDIR-ROOT*"
  (let ((name (format nil "~12,'0D" (random 1000000000000))))
    (pathname (format nil "~A~A/"
                          *workdir-root*
                          name))))

(defun drop-workdir (workdir)
  "Remove all traces of WORKDIR; this is dangerous, please be careful!"
  (uiop:delete-directory-tree workdir :validate t))

(defmacro with-workdir ((var) &body body)
  "Provision a new temporary working directory, storing its PATHNAME in VAR.  BODY will be evaluated with this directory in existence, and then it will be removed."
  `(let ((,var (create-workdir)))
     (unwind-protect
       (progn ,@body)
       (drop-workdir ,var))))
