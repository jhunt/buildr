;;;
;;; src/docker.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines routines for talking to a
;;; Docker v2 registry over HTTP, using JSON.
;;; It properly handles rate-limiting and honors
;;; the X-RateLimit-* headers that upstream sends
;;; in its responses, and inserts SLEEP-delays
;;; when needed.
;;;
;;; For the most part, the only two functions that
;;; are expected to be called by anyone outside of
;;; this file are DOCKER-REGISTRY-CONNECT, which
;;; instantiates a client for communicating with a
;;; v2 registry (complete with credentials), and
;;; DOCKER-TAGS, for looking up defined tags.
;;;

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

(defun docker-registry-connect (&optional url credentials)
  (multiple-value-bind
    (client already-exists?)
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
  "Delay execution until upstream rate limiting resets, as needed"
  (let* ((now   (unix-time))
         (until (slot-value c 'delay-until))
         (nap   (and until (< now until) (- until now))))
    (when nap
      (format t "Docker registry API rate limit reached; sleeping ~Ds~%" nap)
      (sleep nap)
      (setf (slot-value c 'delay-until) nil))))

(defun docker-registry-url (c url)
  "Format and return a full URL, given a client and a partial URL"
  (if (listp url)
    (format nil "~A/~{~A~}"
            (slot-value c 'url)
            url)
    url))

(defun docker-registry-get (c url)
  "Issue a GET request to the upstream registry API"
  (docker-client-delay c)
  (multiple-value-bind
    (body status headers)
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
  "Issue a series of GET requests for paginated endpoints, iterating through all pages and returning the reconstitued list of all results"
  (let* ((r (docker-registry-get c url))
         (body (json:decode-json-from-string (nth-value 0 r)))
         (results (append results (cdr (assoc :results body))))
         (next (cdr (assoc :next body))))
    (if next
      (docker-get-all c next results)
      results)))

(defun parse-image-spec (spec)
  (let ((slashes (count-if #'(lambda (c) (eql #\/ c))
                           spec)))
    (cond ((< slashes 2)
           (values *default-docker-registry*
                   (if (eql 1 slashes)
                       spec
                       (format nil "library/~A" spec))))
          (t
           (let* ((end (position #\/ spec :from-end t))
                  (mid (position #\/ spec :from-end t :end end)))
             (values
               (format nil "https://~A" (subseq spec 0 mid))
               (subseq spec (1+ mid))))))))

(defun by-tag (a b)
  "Compare A and B as lists, where the CAR of each is a SEMVER"
  (semver-<? (first a) (first b)))

(defun docker-tags (image)
  "Retrieve the full list of tags defined for IMAGE from its owning Docker registry"
  (multiple-value-bind
    (url name)
    (parse-image-spec image)

    (let* ((c (docker-registry-connect url))
           (tags (docker-get-all c
                   `("repositories/" ,name "/tags/"))))
      (sort
        (remove-if #'null
          (loop for tag in tags
                for ver = (parse-semver (cdr (assoc :name tag)))
                collecting
                (list
                  (or ver '(0 0 0 0))
                  (cdr (assoc :name tag))
                  (cdr (assoc :tag--last--pushed tag)))))
        #'by-tag))))
