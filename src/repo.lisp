;;;
;;; src/repo.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines routines for interacting
;;; with configured git repositories. Several
;;; of these are wrappers around their GIT-*
;;; counterparts; REPO-CLONE wraps GIT-CLONE to
;;; supply all of the url, branch, and key path
;;; arguments on your behalf, for example.
;;;

(in-package #:buildr)

(defsimpleclass repo
  (url key branch workdir))

(defun repo-path (relative)
  "Formulate a new path, relative to the repository working directory"
  (format nil "~A/~A" (repo-workdir) relative))

(defmacro with-fresh-repo ((var repo) &body body)
  "Allocates a fresh copy of REPO, with a new temporary WORKDIR bound"
  (let ((wd  (gensym))
        (tmp (gensym)))
    `(with-workdir (,wd)
       (let* ((,tmp ,repo)
              (,var (make-repo :url     (repo-url    ,tmp)
                               :key     (repo-key    ,tmp)
                               :branch  (repo-branch ,tmp)
                               :workdir ,wd)))
         ,@body))))

(defun repo-clone (repo)
  "Clone the REPO into its working directory"
  (git-clone (repo-workdir repo)
             (repo-url     repo)
             (repo-branch  repo)
             (repo-key     repo)))

(defun repo-dirty? (repo)
  "Predicate to check if there are changes in WORKDIR that need to be committed"
  (git-dirty? (repo-workdir repo)))

(defun repo-commit (repo msg)
  "Commit all new and modified files in WORKDIR, using MSG as the commit log message"
  (git-commit (repo-workdir repo) msg))

(defun repo-push (repo)
  "Push the git commits from the local working copy to its upstream origin (same branch)"
  (git-push (repo-workdir repo)
            (repo-branch  repo)))

(defun repo-head-sha1 (repo)
  "Get the HEAD SHA1 commit-ish from the working copy"
  (git-head-sha1 (repo-workdir repo)))

(defun repo-read-config (repo)
  "Read the .buildr.yml file from the REPO working directory"
  (let ((raw (ignore-errors
               (yaml:parse
                 (pathname
                   (format nil "~A.buildr.yml"
                           (repo-workdir repo)))))))
    (when raw
      (loop for k being each hash-key   in raw
            for v being each hash-value in raw
            do
            (setf (gethash k raw)
                  `(:target ,(or (gethash "target" v)
                                 "buildr-rebase")
                    :rebase ,(gethash "rebase" v)))))
    raw))
