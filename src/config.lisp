(in-package #:buildr)

(defun read-config (from)
  (let* ((config (read-file from))
         (docker (getf config :docker))
         (keys   (getf config :keys))
         (repos  (getf config :repos)))
    (when docker
      ; pre-connect to all registries
      (loop for registry in docker do
            (docker-registry-connect
              (getf docker :url)
              (getf docker :credentials))))

    (when (and keys repos)
      (loop for repo in repos collecting
            `(:url    ,(getf repo :url)
              :branch ,(or (getf repo :branch)
                           "master")
              :key    ,(getf keys
                             (or (getf repo :key)
                                 :default)))))))
