(in-package #:buildr)

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

(defun docker-client-delay (c)
  (let ((until (slot-value c 'delay-until)))
    (when until
      (let ((nap (- until (unix-time))))
        (format t "Docker registry API rate limit reached; sleeping ~Ds~%" nap)
        (sleep nap))
      (setf (slot-value c 'delay-until) nil))))

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

(defun docker-registry-get (c url)
  (docker-client-delay c)
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
