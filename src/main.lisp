;;;
;;; src/main.lisp
;;; ©2022 James Hunt
;;;
;;; This is the main implementation of BUILDR.
;;; It's sort of the top-level of the application.
;;;
;;; The most interesting routine defined here is
;;; RUN/ALL, which executes a complete cycle of
;;; the BUILDR event loop, on a delay-interval.
;;;
;;; That cycle consists of:
;;;
;;;   1. Reading the configuration of watched
;;;      git repositories, their SSH keys, and
;;;      Docker v2 regisry credentials.
;;;
;;;   2. Cloning watched repositories to determine
;;;      how their owners want them rebased.
;;;
;;;   3. Retrieving the latest image tags from the
;;;      Docker v2 registries where they reside.
;;;
;;;   4. Compiling a list of jobs, the intersection
;;;      of what is possible (tags from step 3) and
;;;      what is desired (config from step 2).
;;;
;;;   5. Executing each build job, and notifying
;;;      appropriate parties.
;;;

(in-package #:buildr)

(defvar *last-build-id*
  nil
  "The last used BUILDR build job ID number")

(defun get-last-build-id ()
  "Retrieve the last build ID, either from memory or from disk"
  (or *last-build-id*
      (ignore-errors
        (with-open-file (in #p".buildr.id")
          (read in)))
      0))

(defun save-last-build-id (id)
  "Save the last build ID to memory and disk"
  (setf *last-build-id* id)
  (with-open-file (out #p".buildr.id")
    (write id :stream out)))

(defun run/1 (config-path)
  "Run a single cycle of the BUILDR event loop, starting with reading configuration (which may have changed since our last run) from CONFIG-PATH and working outward from there"
  (format t "reading configuration from ~A...~%" config-path)
  (let ((config (read-config config-path)))
    (format t "building rules...~%")
    (let ((rules (build-rules config)))
      (format t "retrieving tag inventory...~%")
      (let ((tags (rules-tags rules)))
        (format t "formulating build jobs...~%")
        (multiple-value-bind
          (jobs last-build-id)
          (build-jobs (get-last-build-id) rules tags)

          (save-last-build-id last-build-id)
          (format t "running all ~D jobs...~%"
                  (hash-table-count jobs))
          (loop for job being each hash-value in jobs do
            (run-job job)))))))

(defun run/all (config-path interval &key (via #'timer/sync))
  "Run multiple cycles of the BUILDR event loop, each INTERVAL seconds apart.  The :via keyword argument controls whether the timer loop runs synchronously (default) or via some other periodic executor function"
  (funcall via interval (f* #'run/1 config-path)))
