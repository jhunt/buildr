(in-package #:buildr)

(defun repo-url (repo)
  (getf repo :url))
(defun repo-key (repo)
  (getf repo :key))
(defun repo-branch (repo)
  (getf repo :branch))

(defun repo-clone (repo workdir)
  (git-clone workdir
             (repo-url repo)
             (repo-branch repo)
             (repo-key repo)))

(defun repo-head-sha1 (repo workdir)
  (declare (ignore repo))
  (git-head-sha1 workdir))

(defun repo-push (repo workdir)
  (git-push workdir (repo-branch repo)))

(defun repo-read-config (repo workdir)
  (declare (ignore repo))
  (let ((raw (ignore-errors
               (yaml:parse
                 (pathname
                   (format nil "~A.buildr.yml" workdir))))))
    (when raw
      (loop for k being each hash-key in raw
            for v being each hash-value in raw
            do
            (setf (gethash k raw)
                  `(:target ,(or (gethash "target" v)
                                 "buildr-rebase")
                    :rebase ,(gethash "rebase" v)))))
    raw))
