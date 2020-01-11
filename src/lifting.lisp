(in-package :cl-wavelets)

(declaim (optimize (speed 3))
         (ftype lifting-function
                haar
                cdf-2-2
                cdf-4-2
                inv-haar
                inv-cdf-2-2
                inv-cdf-4-2)
         (type function
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
                         (even-1 (aref-extended array (+ -2 i) :end end))
                         (odd-1  (aref-extended array (+ -1 i) :end end))
                         (even+1 (aref-extended array (+  2 i) :end end))
                         (odd+1  (aref-extended array (+  3 i) :end end)))
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
  (decf even (ash (+ odd odd-1) -2))
  (decf odd (+ even even+1))
  (decf even (- (ash odd -4) (ash (* odd-1 3) -4)))
  (progn
    (incf odd (ash even 1))
    (incf even (ash odd -1))
    (decf odd even)))

(def-lifting-scheme (inv-cdf-4-2 :cdf-4-2 :inverse)
  (progn
    (incf odd even)
    (decf even (ash odd -1))
    (decf odd (ash even 1)))
  (incf even (- (ash odd -4) (ash (* odd-1 3) -4)))
  (incf odd (+ even even+1))  
  (incf even (ash (+ odd odd-1) -2)))

(def-lifting-scheme (cdf-3-1 :cdf-3-1 :forward)
  (decf even (floor odd-1 3))
  (decf odd  (ash (+ (* 9 even)
                       (* 3 even+1))
                  -3))
  (progn
    (incf even (floor (* 2 odd) 9))
    (incf odd  (ash (* 3 even) -1))
    (incf even (floor odd 3))
    (decf odd even)))

(def-lifting-scheme (inv-cdf-3-1 :cdf-3-1 :inverse)
  (progn
    (incf odd even)
    (decf even (floor odd 3))
    (decf odd  (ash (* 3 even) -1))
    (decf even (floor (* 2 odd) 9)))
  (incf odd  (ash (+ (* 9 even)
                     (* 3 even+1))
                  -3))
  (incf even (floor odd-1 3)))

(def-lifting-scheme (cdf-2-2 :cdf-2-2 :forward)
  (decf odd (ash (+ even even+1) -1))
  (incf even (ash (+ odd odd-1) -2)))

(def-lifting-scheme (inv-cdf-2-2 :cdf-2-2 :inverse)
  (decf even (ash (+ odd odd-1) -2))
  (incf odd (ash (+ even even+1) -1)))

(def-lifting-scheme (haar :haar :forward)
  (progn
    (decf odd even)
    (incf even (ash odd -1))))

(def-lifting-scheme (inv-haar :haar :inverse)
  (progn
    (decf even (ash odd -1))
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

(defmacro with-lifting-scheme ((lifting aref-style &key inverse) &body body)
  "Set @c(*lifting-func*) and @c(*aref-func*) parameters to desired
value."
  `(let ((*aref-func* (get-boundary-style-lifting ,aref-style))
         (*lifting-func* (,(if inverse
                               'get-inverse-transform
                               'get-forward-transform)
                           ,lifting)))
     ,@body))
