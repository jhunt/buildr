;;;
;;; src/rules.lisp
;;; ©2022 James Hunt
;;;
;;; This file contains routines and definitions
;;; related to the rules BUILDR parses from the
;;; git repos it watches, and the .buildr.yml
;;; files contained therein.
;;; 

(in-package #:buildr)

(defun parse-rebase-spec (v)
  "Parse a rebase spec into its constituent IMAGE and tag PREFIX parts"
  (let ((it (position #\: v)))
    (if it
      (values (subseq v 0 it)
              (subseq v (1+ it)))
      (values v ""))))

(defun build-rules (repo-config)
  "Build the rules from a list of repository configurations"
  (let ((rules (make-hash-table :test #'equal)))
    (loop for repo in repo-config do
          (format t "checking repo '~A'~%" (repo-url repo))
          (format t "    using key @~A~%"  (repo-key repo))
          (format t "  considering ~A branch~%" (repo-branch repo))

          (with-fresh-repo (r repo)
            (repo-clone repo)
            (format t "HEAD is at ~A~%" (repo-head-sha1 repo))
            (let ((c (repo-read-config repo)))
              (when c
                (format t ".buildr.yml:~%")
                (loop for k being each hash-key   in c
                      for v being each hash-value in c do
                      (loop for spec in (getf v :rebase) do
                            (multiple-value-bind
                              (image prefix)
                              (parse-rebase-spec spec)
                                (track+
                                  (list image (repo-url repo) k)
                                  rules
                                  `(:target ,(getf v :target)
                                    :repo   ,repo
                                    :prefix ,prefix)))))))
            (format t "~%")))
    rules))

(defun rules-images (rules)
  "Extract the unique set of images defined in RULES"
  (loop for image being each hash-key of rules
        collecting image))

(defun rules-tags (rules)
  "Determine the Docker tags for each image defined in RULES"
  (loop for image being each hash-key of rules
        collecting (cons image (docker-tags image))))
