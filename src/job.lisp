(in-package #:buildr)

(defsimpleclass job
  (id repo dockerfile prefix rebase target))

(defun job-add-rebase (job image tag)
  (setf (job-rebase job)
        (append
          (list (cons image tag))
          (job-rebase job))))

(defun rewrite-from-line (line image prefix tag)
  (nth-value 0
    (if (equal "" prefix)
      (ppcre:regex-replace-all
        (format nil "^(FROM\\s+)~A(:.*?)?(\\s.*)?$" image)
        line
        (format nil "\\1~A:~A\\3" image tag))
      (ppcre:regex-replace-all
        (format nil "^(FROM\\s+)~A(:~A\\b.*?)(\\s.*)?$" image prefix)
        line
        (format nil "\\1~A:~A\\3" image tag)))))

(defun rebase-dockerfile (path image prefix tag)
  (let ((temp (format nil "~A~~" path)))
    (with-open-file (in path)
      (with-open-file (out temp :direction :output
                                :if-exists :overwrite
                                :if-does-not-exist :create)
        (when (and in out)
          (loop for line = (read-line in nil nil)
                while line
                do (write-line
                     (rewrite-from-line line image prefix tag)
                     out)))))
    (format t "renaming ~A -> ~A~%" temp path)
    (rename-file temp path)))

(defmacro make-in (workdir &rest args)
  `(list "make" "-C" (namestring ,workdir) ,@args))

(defun run-job (job)
  (format t "running job ~A~%" (job-id job))
  (format t "    in repo ~A~%" (repo-url (job-repo job)))
  (format t "     branch ~A~%" (repo-branch (job-repo job)))
  (format t "    via key ~A~%" (repo-key (job-repo job)))
  (format t "   updating ~A~%" (job-dockerfile job))
  (format t "   rebasing ~S~%" (job-rebase job))
  (loop for (image . tag) in (job-rebase job)
        do (format t "    - ~A => ~A:~A~%" image image tag))
  (with-workdir (wd)
    ; clone the repo
    (repo-clone (job-repo job) wd)

    ; rebase all dockerfiles given known tags
    (loop for (image . tag) in (job-rebase job)
          with path = (format nil "~A/~A" wd (job-dockerfile job))
          do (rebase-dockerfile path image (job-prefix job) tag))

    (when (git-dirty? wd)
      ; commit changes (to get the sha1)
      (git-commit wd
        (with-output-to-string (msg)
          (format msg "[ci/cd] Buildr #~D Base Images Updates~%"
                  (job-id job))
          (format msg "~%")
          (format msg "Updated the following base images in ~S:~%"
                  (job-dockerfile job))
          (format msg "~%")
          (loop for (image . tag) in (job-rebase job)
                do (format msg "  - ~A => ~A:~A"
                           image image tag))))

      ; do the build
      (with-env ((buildr-build-id (job-id-job))
                 (buildr-git-sha1 (git-head-sha1 wd)))
        (multiple-value-bind (stdout exit)
          (system (make-in wd (job-target job)))

          (format t "output:~%~A~%" stdout)
          (format t "exited ~D~%" exit)

          (if (zerop exit)
            ; push the changes if the make succeeded
            (repo-push (job-repo job) wd)))))))

(defun find-appropriate-tag (image stipulation all-tags)
  (let ((spec (parse-semver stipulation))
        (tags (sort-docker-tags
                (cdr (assoc image all-tags :test #'equal))))
        (candidate nil))
    (loop for tag in tags do
          (unless spec
            (setf candidate tag))
          (if (and (first tag)
                   spec
                   (semver-enough? (first tag) spec))
            (setf candidate tag)))
    (and candidate (second candidate))))

(defmacro three-deep (((a b c v) hash) &body body)
  (with-gensyms (hh a-b b-c)
    `(let ((,hh ,hash))
       (loop for ,a being each hash-key in ,hh
             for ,a-b being each hash-value in ,hh
             do
             (loop for ,b being each hash-key in ,a-b
                   for ,b-c being each hash-value in ,a-b
                   do
                   (loop for ,c being each hash-key in ,b-c
                         for ,v being each hash-value in ,b-c
                         do ,@body))))))

(defun build-jobs (last-build-id rules tags)
 (let ((jobs (make-hash-table :test #'equalp)))
    (three-deep ((image repo dockerfile spec) rules)
      (format t "image:      ~A~%" image)
      (format t "repo:       ~A~%" repo)
      (format t "dockerfile: ~A~%" dockerfile)
      (format t "spec:~%    e~S~%" spec)

      (let* ((tag (find-appropriate-tag image
                    (or (getf spec :prefix) "")
                    tags))
             (id  (list repo dockerfile))
             (job (gethash id jobs)))
        (when (not job)
          (incf last-build-id)
          (setf job (make-job :id         last-build-id
                              :repo       (getf spec :repo)
                              :target     (getf spec :target)
                              :prefix     (or (getf spec :prefix) "")
                              :dockerfile dockerfile
                              :rebase     nil))
          (setf (gethash id jobs) job))
        (job-add-rebase job image tag)))
    (values jobs last-build-id)))
