;(in-package #:buildr)
(require 'cl-yaml)

(defun read-file (path)
  (with-open-file (in path)
    (read in)))

(defun read-config (from)
  (let* ((config (read-file from))
         (keys   (getf config :keys))
         (repos  (getf config :repos)))
    (when (and keys repos)
      (loop for repo in repos collecting
            `(:url    ,(getf repo :url)
              :branch ,(or (getf repo :branch)
                           "master")
              :key    ,(getf keys
                             (or (getf repo :key)
                                 :default)))))))

;; ------------ git.lisp ----
(defmacro git (&rest args)
  `(list "git" ,@args))

(defmacro git-in (workdir &rest args)
  `(git "-C" (namestring ,workdir) ,@args))

(defmacro make-in (workdir &rest args)
  `(list "make" "-C" (namestring ,workdir) ,@args))

(defun exec (command)
  (run command))
  ;(uiop:run-program command :input nil :output t))

(defun system (command)
  ;(format t "system: ~S~%" command)
  (uiop:run-program command
    :input nil
    :output '(:string :stripped t)
    :error-output :output
    :ignore-error-status t))

(defun run (command)
  (format t "exec: ~S~%" command)
  (multiple-value-bind (stdout stderr exit)
    (system command)

    (declare (ignore stderr))
    (format t "------------------------~%")
    (format t "~A~%" stdout)
    (format t "--------~%")
    (format t "exited ~D~%" exit)))

(defparameter *workdir-root* "/tmp/buildr/")

(defun create-workdir ()
  (let* ((name (format nil "~12,'0D" (random 1000000000000)))
         (path (pathname (format nil "~A~A/"
                                 *workdir-root*
                                 name))))
;    (ensure-directories-exist path)
    path))

(defun drop-workdir (workdir)
  (uiop:delete-directory-tree workdir :validate t))

(defmacro with-workdir ((var) &body body)
  `(let ((,var (create-workdir)))
     (unwind-protect
       (progn ,@body)
       (drop-workdir ,var))))

(defun foo (&rest decls)
  (mapcar #'gensym decls))

(defun f+ (f g)
  #'(lambda (v) (funcall f (funcall g v))))

(defun just-gensym (&rest ignored)
  (declare (ignore ignored))
  (gensym))

(defun env-name (sym)
  (substitute #\_ #\- (format nil "~Ae" sym)))

(defun env-val (thing)
  (format nil "~A" thing))

(defmacro with-env (decls &body body)
  (let* ((syms (mapcar #'just-gensym decls))
         (vars (mapcar (f+ #'env-name #'car) decls))
         (vals (mapcar (f+ #'env-val #'cadr) decls)))
    `(let ,(mapcar #'(lambda (sym var)
                       `(,sym (uiop:getenv ,var)))
                   syms vars)
       (unwind-protect
         (progn
           ,@(mapcar #'(lambda (var val)
                         `(setf (uiop:getenv ,var) ,val))
                     vars vals)
           ,@body)
         (progn
           ,@(mapcar #'(lambda (var sym)
                         `(setf (uiop:getenv ,var) ,sym))
                     vars syms))))))

(defun git-clone (workdir url branch path-to-key)
  (with-env ((git-ssh-command (format nil "ssh -i ~A" path-to-key)))
    (exec
      (git "clone"
           "--single-branch"
           "--branch" branch
           "--depth" "1"
           "-o" "origin"
           url
           (namestring workdir)))))

(defun git-dirty? (workdir)
  (not (eql "" (system (git-in workdir "ls-files" "-m")))))

(defun git-commit (workdir msg)
  (run (git-in workdir "add" "."))
  (run (git-in workdir "commit" "-a" "-m" msg)))

(defun git-push (workdir branch)
  (run (git-in workdir "lg"))
  (run (git-in workdir "push" "origin" branch)))

(defun git-head-sha1 (workdir &key (abbreviated t))
  (system
    (append
      (git-in workdir "rev-list" "HEAD" "-n1")
      (if abbreviated (list "--abbrev-commit")))))

;; ------------ web ---
(require 'dexador)
(require 'cl-json)

(defparameter *default-docker-registry* "https://hub.docker.com/v2")
(defparameter *default-docker-credentials*
  '("filefrog" . "rY{hEf*Paib?At.hY)ch"))

(defvar *clients* (make-hash-table :test #'equal))
(defclass docker-registry-client ()
 ((delay-until :initform nil)
  (url
   :initarg :url
   :initform *default-docker-registry*)
  (credentials
   :initarg :credentials
   :initform *default-docker-credentials*)))

(defun docker-registry-connect (&key (url *default-docker-registry*)
                                     (credentials *default-docker-credentials*))
  (multiple-value-bind (client already-exists?)
    (gethash url *clients*)

    (cond (already-exists?
           (setf (slot-value client 'credentials) credentials)
           client)

          (t
           (let ((client (make-instance 'docker-registry-client
                                        :url url
                                        :credentials credentials)))
             (setf (gethash url *clients*) client)
             client)))))

(defun docker-registry-url (c url)
  (if (listp url)
    (format nil "~A/~{~A~}"
            (slot-value c 'url)
            url)
    url))

(defparameter *unix-epoch* (encode-universal-time 0 0 0 1 1 1970 0))
(defun unix-time ()
  (- (get-universal-time)
     *unix-epoch*))

(defun sleep-until (ts)
  (format t "Docker registry API rate limit reached; sleeping ~Ds~%"
          (- ts (unix-time)))
  (sleep (- ts (unix-time))))

(defun docker-registry-get (c url)
  (when (slot-value c 'delay-until)
    (sleep-until (slot-value c 'delay-until))
    (setf (slot-value c 'delay-until) nil))
  (multiple-value-bind (body status headers)
    (dex:get (docker-registry-url c url)
             :basic-auth (slot-value c 'credentials)
             :verbose t)

    (ignore-errors
      (let ((left  (parse-integer (gethash "x-ratelimit-remaining" headers)))
            (reset (parse-integer (gethash "x-ratelimit-reset" headers))))
        (if (zerop left)
          (setf (slot-value c 'delay-until) reset))))

    (values body status headers)))

(defun docker-get-all (c url &optional (results nil))
  (let* ((r (docker-registry-get c url))
         (body (json:decode-json-from-string (nth-value 0 r)))
         (results (append results (cdr (assoc :results body))))
         (next (cdr (assoc :next body))))
    (if next
      (docker-get-all c next results)
      results)))

(defun let+-decls (decls body)
  (cond (decls
          `(let ((,(caar decls)
                  ,(cadar decls)))
             (when ,(caar decls)
               ,(let+-decls (cdr decls) body))))
        (t `(progn ,@body))))

(defmacro let+ (decls &body body)
  (let+-decls decls body))

(defun semver-major (v) (first v))
(defun semver-minor (v) (second v))
(defun semver-revision (v) (third v))
(defun semver-epoch (v) (fourth v))

(defun semver-valid? (v)
  (and (semver-major v)
       (semver-minor v)
       (semver-revision v)
       (semver-epoch v)))

(defun parse-semver (v)
  (let* ((dot1   (position #\. v))
         (dot1n (and dot1  (1+ dot1)))
         (dot2  (and dot1n (position #\. v :start dot1n)))
         (dot2n (and dot2  (1+ dot2))))
    (when (not (and dot2n (position #\. v :start dot2n)))
      (let ((semver (ignore-errors
                      (list
                        (parse-integer (subseq v 0 dot1))
                        (and dot1n (parse-integer (subseq v dot1n dot2)))
                        (and dot2n (parse-integer (subseq v dot2n)))
                        0))))
        (if semver
          ; avoid inadvertantly treating YYYYMMDD datestamps
          ; as major versions; they conflict with actual semver
          ; insofar as 3.x is never newer than 20220125.0.0
          (if (and (> (semver-major semver) (* 1000 100 100))
                   (null (semver-minor semver))
                   (null (semver-revision semver)))
            (list 0 0 0 (semver-major semver))
            semver))))))

(defun semver/- (a b)
  (- (or a 0)
     (or b 0)))

(defun semver-enough? (v spec)
  "Check if V meets the minimum semantic version requirement laid out in SPEC; NIL values in SPEC will be considered as always matching."
  (labels ((vdiff (fn)
             (let ((v (apply fn (list v)))
                   (spec (apply fn (list spec))))
               (- (or v 0) (or spec v 0)))))
    (and (zerop (vdiff #'semver-major))
         (zerop (vdiff #'semver-minor))
         (zerop (vdiff #'semver-revision))
         (zerop (vdiff #'semver-epoch)))))

(defmacro ziggurat (factor &rest terms)
  (let ((n 1))
    `(+ ,@(mapcar #'(lambda (term)
                      (prog1
                        `(* ,n ,term)
                        (setf n (* factor n))))
                  (reverse terms)))))

(defun semver-numeric (v)
  (if (not v) 0
    (ziggurat 100000000
              (1+ (or (semver-major v) -1))
              (1+ (or (semver-minor v) -1))
              (1+ (or (semver-revision v) -1))
              (1+ (or (semver-epoch v) -1)))))

(defun semver-<? (a b)
  (< (semver-numeric a)
     (semver-numeric b)))

;;;  (let ((diff-1 (semver/- (semver-major a) (semver-major b)))
;;;        (diff-2 (semver/- (semver-minor a) (semver-minor b)))
;;;        (diff-3 (semver/- (semver-revision a) (semver-revision b))))
;;;    (or (< diff-1 0)
;;;        (and (zerop diff-1)
;;;             (or (< diff-2 0)
;;;                 (and (zerop diff-2)
;;;                      (< diff-3 0)))))))

(defun docker-image-name-proper (image)
  (if (position #\/ image)
    image
    (format nil "library/~A" image)))

(defun sort-docker-tags (tags)
  (sort tags
    #'(lambda (a b)
        (semver-<? (car a) (car b)))))

(defun docker-tags (c image)
  (let* ((name (docker-image-name-proper image))
         (tags (docker-get-all c `("repositories/" ,name "/tags/"))))
    (sort-docker-tags
      (remove-if #'null
        (loop for tag in tags
              for ver = (parse-semver (cdr (assoc :name tag)))
              collecting
              (list
                (or ver '(0 0 0 0))
                (cdr (assoc :name tag))
                (cdr (assoc :tag--last--pushed tag))))))))

;; ------------ run ---

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

(defun parse-rebase-spec (v)
  (let ((it (position #\: v)))
    (if it
      (values (subseq v 0 it)
              (subseq v (1+ it)))
      (values v ""))))

(defun parse-rebase-prefix (v)
  (nth-value 1 (parse-rebase-spec v)))

(defclass plan ()
  ((data :initform (make-hash-table :test #'equal))))

(defun make-plan ()
  (make-instance 'plan))

(defun track* (key hash value keys)
  (let ((h (gethash key hash)))
    (cond ((null keys)
           (setf (gethash key hash) value))
          ((null h)
           (let ((h (make-hash-table :test #'equal)))
             (setf (gethash key hash) h)
             (track* (car keys) h value (cdr keys)))))))

(defun track+ (keys hash value)
  (if keys
    (track* (car keys) hash value (cdr keys))))

(defun get+ (keys hash)
  (cond ((null keys) nil)
        ((null (cdr keys))
         (gethash (car keys) hash))
        (t
         (get+ (cdr keys)
               (gethash (car keys) hash)))))

(defun build-rules-set (repo-config)
  (let ((rules-set (make-hash-table :test #'equal)))
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
                                    rules-set
                                    `(:target ,(getf v :target)
                                      :repo   ,repo
                                      :prefix ,(parse-rebase-prefix spec)))))))
            (format t "~%")))
    rules-set))

(defun dump-rules-set (rules-set)
  (loop for image being each hash-key of rules-set
        for repos being each hash-value of rules-set
        do
        (loop for repo being each hash-key of repos
              for specs being each hash-value of repos
              do
              (loop for dockerfile being each hash-key of specs
                    for spec       being each hash-value of specs
                    do
                    (format t "~A:~A [~A] = ~S~%"
                            image repo dockerfile spec)))))

(defun rules-set-images (rules-set)
  (loop for image being each hash-key of rules-set
        collecting image))

(defun rules-set-tags (rules-set)
  (loop for image being each hash-key of rules-set
        collecting
        (cons image
              (docker-tags (docker-registry-connect)
                           image))))

(defmacro with-gensyms (vars &body body)
  `(let (,@(mapcar #'(lambda (v)
                       `(,v (gensym)))
                   vars))
     ,@body))

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

(defun keywordf (&rest args)
  (intern
    (format nil "~:@(~{~A~}~)" args)
    'keyword))

(defun internf (&rest args)
  (intern
    (format nil "~:@(~{~A~}~)" args)))

(defmacro defsimpleclass (name fields)
  `(progn
     (defclass ,name ()
       ,(mapcar #'(lambda (f)
                    `(,f :initarg ,(keywordf f)
                         :accessor ,(internf name "-" f)))
                fields))
     (defmacro ,(internf "make-" name) (&rest args)
       `(make-instance ',',name ,@args))))

(defsimpleclass job (id repo dockerfile prefix rebase target))
(defun job-add-rebase (job image tag)
  (setf (job-rebase job)
        (append
          (list (cons image tag))
          (job-rebase job))))

(defun build-jobs (last-build-id rules-set tags)
 (let ((jobs (make-hash-table :test #'equalp)))
    (three-deep ((image repo dockerfile spec) rules-set)
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
        (multiple-value-bind (stdout stderr exit)
          (system (make-in wd (job-target job)))
          (declare (ignore stderr))

          (format t "output:~%~A~%" stdout)
          (format t "exited ~D~%" exit)

          (if (zerop exit)
            ; push the changes if the make succeeded
            (repo-push (job-repo job) wd)))))))

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

(defun dump-jobs (jobs)
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

(defun for (n units)
  (case units
    ((s sec second seconds) n)
    ((m min minute minutes) (* n 60))
    ((h hr  hour   hours)   (* n 60 60))
    ((d     day    days)    (* n 60 60 24))
    (otherwise (error (format nil "unrecognized unit ~S" units)))))

(defun timer/sync (period fn)
  (loop do
    (apply fn '())
    (sleep period)))

(require 'bordeaux-threads)
(defun timer/async (period fn)
  (let ((thread (bordeaux-threads:make-thread
                  #'(lambda ()
                      (timer/sync period fn)))))
    #'(lambda ()
        (bordeaux-threads:destroy-thread thread))))

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
    (format t "building rules set...~%")
    (let ((rules (build-rules-set config)))
      (format t "retrieving tag inventory...~%")
      (let ((tags (rules-set-tags rules)))
        (format t "formulating build jobs...~%")
        (multiple-value-bind
          (jobs last-build-id)
          (build-jobs (get-last-build-id) rules tags)

          (save-last-build-id last-build-id)
          (format t "running all ~D jobs...~%"
                  (length jobs))
          (loop for job being each hash-value in jobs do
            (run-job job)))))))

(defun curry (fn &rest fixed-args)
  #'(lambda (&rest variable-args)
      (apply fn (append fixed-args
                        variable-args))))

(defun run/all (config-path interval &key (via #'timer/sync))
  (funcall via interval (curry #'run/1 config-path)))

;(run/all #p"repos" 

#|
(defvar *rules* nil)
(defvar *jobs* nil)
(defvar *tags* nil)
(defun here (path)
  (merge-pathnames *default-pathname-defaults* path))
(defun dump-tags (tags &optional (from (here #p"dumped")))
  (with-open-file (out from :direction :output
                            :if-exists :overwrite
                            :if-does-not-exist :create)
    (write tags :stream out)))
(defun hydrate (&optional (from #p"dumped"))
  (with-open-file (in from)
    (setf *tags* (read in))))
(defun re-tags ()
  (setf *tags* (rules-set-tags *rules*))
  (dump-tags *tags*))
(defun exercise ()
  (hydrate)
  (let* ((rules   (build-rules-set (read-config #p"repos")))
         (jobs    (build-jobs 42 rules *tags*)))
    (setf *rules* rules)
    (setf *jobs* jobs)))
(defun run-all-jobs ()
  (loop for job being each hash-value in *jobs* do
        (run-job job)))
|#
