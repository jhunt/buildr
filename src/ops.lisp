;;;
;;; src/ops.lisp
;;; ©2022 James Hunt
;;;
;;; This file contains operational routines
;;; intended to be used over something like a
;;; SWANK connection, for field service.
;;;

(in-package #:buildr)

(defun dump-jobs (jobs)
  "Print out pertinent job information for a list of JOBS"
  (loop for (repo dockerfile) being each hash-key of jobs
        for job being each hash-value of jobs
        do
        (format t "[~D] build ~A, in ~A:~%"
                (job-id job) dockerfile repo)
        (format t "    branch ~A, key ~A~%"
                (repo-branch (job-repo job))
                (repo-key (job-repo job)))
        (format t "      via `make ~A`~%"
                (job-target job))
        (format t "~{  - ~A~%~}" (job-rebase job))))

(defun dump-rules (rules)
  "Print out pertinent build rule information for a RULES list"
  (loop for image being each hash-key of rules
        for repos being each hash-value of rules
        do
        (loop for repo being each hash-key of repos
              for specs being each hash-value of repos
              do
              (loop for dockerfile being each hash-key of specs
                    for spec       being each hash-value of specs
                    do
                    (format t "~A:~A [~A] = ~S~%"
                            image repo dockerfile spec)))))
