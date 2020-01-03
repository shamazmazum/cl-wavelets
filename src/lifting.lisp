(in-package :cl-wavelets)

(declaim (optimize (speed 3))
         (ftype lifting-function
                haar
                cdf-2-2
                cdf-4-2
                inv-haar
                inv-cdf-2-2
                inv-cdf-4-2)
         #l"SBCL-1.5.0"
         (type lifting-function
               *lifting-func*))

(defparameter *forward-table* nil
  "Table of wavelets used for forward transform")

(defparameter *inverse-table* nil
  "Table of wavelets used for inverse transform")

(defmacro def-lifting-scheme ((name alias direction) &body body)
  "Define a function with the name `NAME` which performs one step of
discrete wavelet transform (DWT, PWT) using lifting scheme. `ALIAS`
is a keyword associated with this function which can be later used
with `dwt` or `dwt-inverse` functions. Direction must be either
`:FORWARD` or `INVERSE`.

The body of the lifting scheme consists of several expressions, each
such expression represents a lifting step (called prediction and
update in literature). In each expression you can manipulate with
`ODD`, `EVEN`, `ODD-1`, `EVEN-1`, `ODD+1` and `EVEN+1` variables,
representing the odd sample, the even sample, the previous and the
next odd/even samples respectively.

TODO: Allow access to n-th odd or even sample."
  `(progn
     (declaim (ftype lifting-function ,name))
     (defun ,name (array &key (start 0) (end (length array)))
       (declare (type (sa-sb 32) array)
                (type non-negative-fixnum start end))
       (symbol-macrolet ((even (aref array (+ 0 i)))
                         (odd  (aref array (+ 1 i)))
                         (even-1 (funcall *aref-func* array (+ -2 i) :end end))
                         (odd-1  (funcall *aref-func* array (+ -1 i) :end end))
                         (even+1 (funcall *aref-func* array (+  2 i) :end end))
                         (odd+1  (funcall *aref-func* array (+  3 i) :end end)))
         ,@(loop for expr in body collect
                `(loop for i from start below end by 2 do
                      ,expr)))
       array)
     (pushnew (cons ,alias #',name)
              ,(ecase direction
                 (:forward '*forward-table*)
                 (:inverse '*inverse-table*))
              :key #'car)))

(def-lifting-scheme (cdf-4-2 :cdf-4-2 :forward)
  (decf even (truncate (+ odd odd-1) 4))
  (decf odd (+ even even+1))
  (decf even (- (truncate odd 16) (truncate (* odd-1 3) 16)))
  (progn
    (incf odd (* even 2))
    (incf even (truncate odd 2))
    (decf odd even)))

(def-lifting-scheme (inv-cdf-4-2 :cdf-4-2 :inverse)
  (progn
    (incf odd even)
    (decf even (truncate odd 2))
    (decf odd (* even 2)))
  (incf even (- (truncate odd 16) (truncate (* odd-1 3) 16)))
  (incf odd (+ even even+1))  
  (incf even (truncate (+ odd odd-1) 4)))

(def-lifting-scheme (cdf-2-2 :cdf-2-2 :forward)
  (decf odd (truncate (+ even even+1) 2))
  (incf even (truncate (+ odd odd-1) 4)))

(def-lifting-scheme (inv-cdf-2-2 :cdf-2-2 :inverse)
  (decf even (truncate (+ odd odd-1) 4))
  (incf odd (truncate (+ even even+1) 2)))  

(def-lifting-scheme (haar :haar :forward)
  (progn
    (decf odd even)
    (incf even (truncate odd 2))))

(def-lifting-scheme (inv-haar :haar :inverse)
  (progn
    (decf even (truncate odd 2))
    (incf odd even)))

(defparameter *lifting-func* #'haar
  "Current wavelet transform function")

(defun get-forward-transform (symbol)
  (declare (type symbol symbol))
  (or (cdr (assoc symbol *forward-table*))
      (error "Unknown forward wavelet transform: ~a" symbol)))

(defun get-inverse-transform (symbol)
  (declare (type symbol symbol))
  (or (cdr (assoc symbol *inverse-table*))
      (error "Unknown inverse wavelet transform: ~a" symbol)))

(defun get-wavelets ()
  "Get a list of available wavelets"
  (mapcar #'car *forward-table*))

;; TODO: replace with compiler-macro?
(defun phase-split (array &key
                            (start 0)
                            (end (length array))
                            inverse
                            tmp)
  "Split even and odd samples in the array. If INVERSE is T, higher
frequencies subband will be stored before lower frequencies subband."
  (declare (type (sa-sb 32) array)
           (type non-negative-fixnum start end)
           (type boolean inverse))
  (let* ((half (/ (- end start) 2))
         (tmp (if tmp tmp (make-array half :element-type '(sb 32)))))
    (declare (type non-negative-fixnum half)
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
           (type non-negative-fixnum start end))
  (let* ((half (/ (- end start) 2))
         (tmp (if tmp tmp (make-array half :element-type '(sb 32)))))
    (declare (type non-negative-fixnum half)
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
  (declare (type positive-fixnum length))
  (make-array (/ length 2) :element-type '(signed-byte 32)))
