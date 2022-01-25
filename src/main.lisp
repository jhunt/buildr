(in-package #:buildr)

(defvar *last-build-id* nil)

(defun get-last-build-id ()
  (or *last-build-id*
      (ignore-errors
        (with-open-file (in #p".buildr.id")
          (read in)))
      0))

(defun save-last-build-id (id)
  (setf *last-build-id* id)
  (with-open-file (out #p".buildr.id")
    (write id :stream out)))

(defun run/1 (config-path)
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
  (funcall via interval (f* #'run/1 config-path)))
