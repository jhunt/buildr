;;;
;;; src/semver.lisp
;;; ©2022 James Hunt
;;;
;;; This file defines routines for handling
;;; semantic versions, including non-semver
;;; quantities like epochal timestamp versions.
;;;
;;; It provides parsing and validation through
;;; the likes of PARSE-SEMVER and SEMVER-VALID?
;;;

(in-package #:buildr)

(defun semver-major (v)
  "Return the MAJOR version component of the semantic version V"
  (first v))

(defun semver-minor (v)
  "Return the MINOR version component of the semantic version V"
  (second v))

(defun semver-revision (v)
  "Return the REVISION component of the semantic version V"
  (third v))

(defun semver-epoch (v)
  "Return the EPOCH component of the semantic version V"
  (fourth v))

(defun semver-valid? (v)
  "Check if V is a valid and fully-specified semantic version"
  (and (semver-major    v)
       (semver-minor    v)
       (semver-revision v)
       (semver-epoch    v)))

(defun parse-semver (v)
  "Parse V (a string) into a semantic version, assuming dot notation"
  (let* ((dot1  (position #\. v))
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
  "Perform a component subtraction, treating NIL as 0"
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
  "Build a stepwise numeric tower by adding consecutive terms, each multiple by a linearly growing factor"
  (let ((n 1))
    `(+ ,@(mapcar #'(lambda (term)
                      (prog1
                        `(* ,n ,term)
                        (setf n (* factor n))))
                  (reverse terms)))))

(defun semver-numeric (v)
  "Convert V into a number, by treating each term (major, minor, revision, epoch) as an 8-digit substring of the larger number"
  (if (not v) 0
    (ziggurat 100000000
              (1+ (or (semver-major v) -1))
              (1+ (or (semver-minor v) -1))
              (1+ (or (semver-revision v) -1))
              (1+ (or (semver-epoch v) -1)))))

(defun semver-<? (a b)
  "Determine if the semantic version A is logically less than B"
  (< (semver-numeric a)
     (semver-numeric b)))
