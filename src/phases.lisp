(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(sera:-> check-power-of-2-p
         (alex:non-negative-fixnum)
         (values alex:non-negative-fixnum &optional))
(defun check-power-of-2 (x)
  "Check if the argument is a power of two."
  (declare (type alex:non-negative-fixnum x))
  (if (zerop (logand x (1- x))) x
      (error "Argument must be power of 2")))

;; TODO: replace with compiler-macro?
(defun phase-split (array &key
                            (start 0)
                            (end (length array))
                            inverse
                            tmp)
  "Split even and odd samples in the array. If INVERSE is T, higher
frequencies subband will be stored before lower frequencies subband."
  (declare (type (sa-sb 32) array)
           (type alex:non-negative-fixnum start end)
           (type boolean inverse))
  (let* ((half (/ (- end start) 2))
         (tmp (if tmp tmp (make-array half :element-type '(sb 32)))))
    (declare (type alex:non-negative-fixnum half)
             (type (sa-sb 32) tmp))
    ;; Copy odd samples to tmp
    (loop
       for i fixnum from start below end by 2
       for j fixnum from 0 by 1
       do
         (setf (aref tmp j)
               (aref array (+ (if inverse 0 1) i))))
    ;; Move evens
    (loop
       for i from start below end by 2
       for j from start by 1
       do
         (setf (aref array j)
               (aref array (+ (if inverse 1 0) i))))
    ;; Move odds
    (loop
       for i fixnum below half
       for j fixnum from start by 1
       do
         (setf (aref array (+ j half))
               (aref tmp i))))
  array)

(defun phase-mix (array &key (start 0) (end (length array)) tmp)
  "Interleave even and odd samples in the array"
  (declare (type (sa-sb 32) array)
           (type alex:non-negative-fixnum start end))
  (let* ((half (/ (- end start) 2))
         (tmp (if tmp tmp (make-array half :element-type '(sb 32)))))
    (declare (type alex:non-negative-fixnum half)
             (type (sa-sb 32) tmp))
    ;; Copy even samples to tmp
    (loop
       for i below half
       for j from start by 1
       do
         (setf (aref tmp i)
               (aref array j)))
    ;; Move odds
    (loop
       for i fixnum from start below end by 2
       for j fixnum from start by 1
       do
         (setf (aref array (1+ i))
               (aref array (+ j half))))
    ;; Move evens
    (loop
       for i below half
       for j from start by 2
       do
         (setf (aref array j)
               (aref tmp i))))
  array)

(defun make-tmp-array (length)
  "Make temporary array for PHASE-SPLIT and PHASE-MIX functions"
  (declare (type alex:positive-fixnum length))
  (make-array (/ length 2) :element-type '(signed-byte 32)))
