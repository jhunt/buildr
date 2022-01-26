;;;
;;; src/config.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines routines related to reading
;;; the configuration of a BUILDR instance.
;;;
;;; BUILDR is configured through a single file that
;;; contains a single, top-level S-expression.
;;; It looks something like this:
;;;
;;; (:keys    ;; SSH keys for cloning repos
;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (:key-name "/path/to/key"
;;;   :another-key "/another/path")
;;;
;;;
;;;  :repos   ;; Git repositories to watch
;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ((:url    "git@github.com:/the/repo"
;;;    :key    :key-name
;;;    :branch "develop")
;;;   (:url    "git@elsewhere:repo.git"))
;;;
;;;
;;;  :docker  ;; Docker registries & creds
;;;  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ((:url "https://hub.docker.com/v2"
;;;    :username "your-user"
;;;    :password "it$-a-SEKRIT")
;;;   (:url "https://private.regi.st.ry/"
;;;    :username "..."
;;;    :password "...")))
;;;
;;;
;;; First up, the :keys section defines SSH
;;; private keys, binding aliases (keywords)
;;; to their (absolute) paths on-disk.  These
;;; keys will be referenced from the :repos
;;; definitions that follow.  It is expected
;;; that you will have far fewer keys than
;;; git repositories.
;;;
;;; Next, the :repos section defines a list
;;; of the git repositories that BUILDR will
;;; clone and check for .buildr.yml rules.
;;; Each repository is defined by its clone
;;; URL (:url), which key to authenticate
;;; with (:key, defaulting to :default), and
;;; which branch to consider (:branch, which
;;; defaults to "master").
;;;
;;; Finally, the :docker section identifies
;;; what credentials to use when communicating
;;; to one or more Docker v2 registries.
;;;

(in-package #:buildr)

(defun read-config (path)
  "Reads in BUILDR configuration from the file at PATH"
  (let* ((config (read-file path))
         (docker (getf config :docker))
         (keys   (getf config :keys))
         (repos  (getf config :repos)))
    (when docker
      ; pre-connect to all registries
      (loop for registry in docker do
            (docker-registry-connect
              (getf docker :url)
              (cons (getf docker :username)
                    (getf docker :password)))))

    (when (and keys repos)
      (loop for repo in repos collecting
            (make-repo
              :url    (getf repo :url)
              :branch (or (getf repo :branch)
                          "master")
              :key    (getf keys
                            (or (getf repo :key)
                                :default)))))))
