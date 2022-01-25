(in-package #:buildr)

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
