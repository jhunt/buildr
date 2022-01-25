(in-package #:buildr)

(defparameter *default-docker-registry*
  "https://hub.docker.com/v2"
  "The default Docker Registry API endpoint to connect to")

(defvar *docker-clients*
  (make-hash-table :test #'equal)
  "Docker clients we have already sent requests from, indexed by URL.  We keep track of these so that we may remember the rate-limiting delay requirements, to avoid getting blocked.")

(defclass docker-registry-client ()
 ((delay-until :initform nil)
  (url
   :initarg :url
   :initform *default-docker-registry*)
  (credentials
   :initarg :credentials
   :initform nil)))

(defun docker-registry-connect (&optional (url *default-docker-registry*)
                                          credentials)
  (multiple-value-bind (client already-exists?)
    (gethash url *docker-clients*)

    (cond (already-exists?
           (setf (slot-value client 'credentials) credentials)
           client)

          (t
           (let ((client (make-instance 'docker-registry-client
                                        :url url
                                        :credentials credentials)))
             (setf (gethash url *docker-clients*) client)
             client)))))

(defun docker-client-delay (c)
  (let ((until (slot-value c 'delay-until)))
    (when until
      (let ((nap (- until (unix-time))))
        (format t "Docker registry API rate limit reached; sleeping ~Ds~%" nap)
        (sleep nap))
      (setf (slot-value c 'delay-until) nil))))

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

(defun docker-registry-for (image)
  (let ((slashes (count-if #'(lambda (c) (eql #\/ c))
                           image)))
    (cond ((< slashes 2)
           *default-docker-registry*)
          (t
           (format nil "https://~A"
                   (subseq image 0
                     (position #\/ image :from-end t
                       :end (position #\/ image :from-end t))))))))

(defun by-tag (a b)
  (semver-<? (first a) (first b)))

(defun docker-tags (image)
  (let* ((c (docker-registry-connect
              :url (docker-registry-for image)))
         (name (docker-image-name-proper image))
         (tags (docker-get-all c `("repositories/" ,name "/tags/"))))
    (sort
      (remove-if #'null
        (loop for tag in tags
              for ver = (parse-semver (cdr (assoc :name tag)))
              collecting
              (list
                (or ver '(0 0 0 0))
                (cdr (assoc :name tag))
                (cdr (assoc :tag--last--pushed tag)))))
      #'by-tag)))
