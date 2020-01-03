(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defun check-length (length)
  "Check if the argument is a power of two."
  (declare (type non-negative-fixnum length))
  (if (zerop (logand length (1- length)))
      length
      (error "Length must be power of 2")))
