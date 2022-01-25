(in-package #:buildr)

(defparameter *unix-epoch*
  (encode-universal-time 0 0 0 1 1 1970 0)
  "The UNIX Epoch, corresponding to January 1st 1970, at midnight UTC")

(defun unix-time ()
  "Return the current timestamp, expressed as seconds since the UNIX Epoch, Jan 1 1970 00:00:00Z"
  (- (get-universal-time)
     *unix-epoch*))

(defun timer/sync (period fn)
  "Runs FN every PERIOD seconds"
  (loop do
    (apply fn '())
    (sleep period)))

(defun timer/async (period fn)
  "Runs FN every PERIOD seconds in a background thread, returning a LAMBDA that can be called to halt the thread"
  (let ((thread (bordeaux-threads:make-thread
                  #'(lambda ()
                      (timer/sync period fn)))))
    #'(lambda ()
        (bordeaux-threads:destroy-thread thread))))
