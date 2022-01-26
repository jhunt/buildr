;;;
;;; src/git.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines routines and macros for
;;; dealing with the git version control system
;;; through its `git' command-line utility.
;;;
;;; It relies heavily on the primitives defined
;;; in src/exec.lisp; notably RUN, SYSTEM, and
;;; WITH-ENV.
;;;

(in-package #:buildr)

(defmacro git (&rest args)
  "Prepends the `git' command to the beginning of ARGS"
  `(list "git" ,@args))

(defmacro git-in (workdir &rest args)
  "Sets up a git command invocation to take place in the given WORKDIR"
  `(git "-C" (namestring ,workdir) ,@args))

(defun git-clone (workdir url branch path-to-key)
  "Clone the git repository at URL into WORKDIR, pulling only BRANCH"
  (with-env ((git-ssh-command (format nil "ssh -i ~A" path-to-key)))
    (run
      (git "clone"
           "--single-branch"
           "--branch" branch
           "--depth" "1"
           "-o" "origin"
           url
           (namestring workdir)))))
(defun git-dirty? (workdir)
  "Predicate to check if there are changes in WORKDIR that need to be committed"
  (not (eql "" (system (git-in workdir "ls-files" "-m")))))

(defun git-commit (workdir msg)
  "Commit all new and modified files in WORKDIR, using MSG as the commit log message"
  (run (git-in workdir "add" "."))
  (run (git-in workdir "commit" "-a" "-m" msg)))

(defun git-push (workdir branch)
  "Print the last 20 commits (locally) in WORKDIR, then push BRANCH to origin"
  (run (git-in workdir "log" "-n" "20"
                             "--pretty=format:[%ci] %h %s (%cN)"))
  (run (git-in workdir "push" "origin" branch)))

(defun git-head-sha1 (workdir &key (abbreviated t))
  "Return the SHA1 commit ID of HEAD in WORKDIR"
  (system
    (append
      (git-in workdir "rev-list" "HEAD" "-n1")
      (if abbreviated (list "--abbrev-commit")))))
