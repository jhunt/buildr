#|
 | buildr.asd
 | a docker image rebuilder
 | © 2022 James Hunt
 | 25 Jan 2022
 |#
(let ((me "James Hunt <james@jameshunt.us>"))
  (asdf:defsystem #:buildr
    :version "0.0.1" :license "mit"
    :author me :maintainer me
    :description "a docker image rebuilder"
    :homepage "https://jameshunt.us/p/buildr"
    :serial t
    :depends-on (:bordeaux-threads
                 :cl-json
                 :cl-yaml
                 :dexador)
    :components ((:module src
                  :components ((:file "package")
                               (:file "utilities")
                               (:file "exec")
                               (:file "workdir")
                               (:file "time")
                               (:file "semver")
                               (:file "docker")
                               (:file "git")
                               (:file "repo")
                               (:file "job")
                               (:file "rules")
                               (:file "config")
                               (:file "main")
                               (:file "ops"))))))
