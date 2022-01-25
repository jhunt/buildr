(in-package #:buildr)

(defun parse-rebase-spec (v)
  (let ((it (position #\: v)))
    (if it
      (values (subseq v 0 it)
              (subseq v (1+ it)))
      (values v ""))))

(defun parse-rebase-prefix (v)
  (nth-value 1 (parse-rebase-spec v)))

(defun build-rules (repo-config)
  (let ((rules (make-hash-table :test #'equal)))
    (loop for repo in repo-config do
          (format t "checking repo '~A'~%" (repo-url repo))
          (format t "    using key @~A~%"  (repo-key repo))
          (format t "  considering ~A branch~%" (repo-branch repo))

          (with-workdir (wd)
            (repo-clone repo wd)
            (format t "HEAD is at ~A~%" (repo-head-sha1 repo wd))
            (let ((c (repo-read-config repo wd)))
              (when c
                (format t ".buildr.yml:~%")
                (loop for k being each hash-key in c
                      for v being each hash-value in c
                      do
                      (loop for spec in (getf v :rebase)
                            do
                            (track+ (list
                                      (parse-rebase-spec spec)
                                      (repo-url repo)
                                      k)
                                    rules
                                    `(:target ,(getf v :target)
                                      :repo   ,repo
                                      :prefix ,(parse-rebase-prefix spec)))))))
            (format t "~%")))
    rules))

(defun rules-images (rules)
  (loop for image being each hash-key of rules
        collecting image))

(defun rules-tags (rules)
  (loop for image being each hash-key of rules
        collecting
        (cons image
              (docker-tags (docker-registry-connect)
                           image))))
