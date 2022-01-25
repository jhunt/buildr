(in-package #:buildr)

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
