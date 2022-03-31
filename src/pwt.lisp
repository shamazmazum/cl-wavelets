(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defstruct pwt-node
  data
  low-pass
  high-pass
  (cost 0 :type fixnum)
  (preferred nil :type boolean))

(declaim (type alex:positive-fixnum *threshold*))
(defparameter *threshold* 1
  "Default value for an argument to @c(make-threshold-cost).")

(defun make-threshold-cost (&optional (threshold *threshold*))
  "Make a cost function which depends on the number of items whose
  magnitude is greater than @c(threshold)."
  (declare (type alex:positive-fixnum threshold))
  (flet ((threshold-cost (array)
           (declare (type (sa-sb 32) array))
           (count-if
            (lambda (x)
              (declare (type (sb 32) x))
              (> (abs x) threshold))
            array)))
    #'threshold-cost))

;; Declaration of more specific type does not work on old SBCL
;; versions.
(declaim (type #+nil (function
                      ((sa-sb 32))
                      (values fixnum &optional))
               #-nil function
               *cost-func*))
(defparameter *cost-func* (make-threshold-cost))

;; TODO: this function can be optimized because all tree is rarely
;; needed. You can stop the tree calculation when the cost of a node
;; is less or equal to the total cost of its children. Now PWT
;; algorithm is performed as if the worst case is occured.
(defun build-tree (array)
  (declare (type (sa-sb 32) array))
  (let ((len (length array))
        (node (make-pwt-node :data (copy-seq array)
                             :cost (funcall *cost-func* array))))
    (cond
      ((> len 1)
       (funcall *lifting-func* array)
       (phase-split array)
       (setf (pwt-node-low-pass node)
             (build-tree (subseq array 0 (/ len 2)))
             (pwt-node-high-pass node)
             (build-tree (subseq array (/ len 2))))
       (if (<= (pwt-node-cost node)
               (+ (pwt-node-cost
                   (pwt-node-low-pass node))
                  (pwt-node-cost
                   (pwt-node-high-pass node))))
           (setf (pwt-node-preferred node) t)))
      (t
       (setf (pwt-node-preferred node) t)))
    node))

(defun get-basis-key (node)
  (declare (type pwt-node node))
  (if (not (pwt-node-preferred node))
      (list
       (get-basis-key
        (pwt-node-low-pass node))
       (get-basis-key
        (pwt-node-high-pass node)))))

(defun collect-preferred (node &optional acc)
  (declare (type pwt-node node))
  (if (pwt-node-preferred node)
      (cons (pwt-node-data node) acc)
      (collect-preferred
       (pwt-node-low-pass node)
       (collect-preferred
        (pwt-node-high-pass node)
        acc))))

(defun concatenate-into (array array-list)
  (declare (type (sa-sb 32) array))
  (let ((i 0))
    (declare (type alex:non-negative-fixnum i))
    (dolist (part array-list)
      (declare (type (sa-sb 32) part))
      (dotimes (j (length part))
        (declare (type alex:non-negative-fixnum j))
        (setf (aref array i)
              (aref part j))
        (incf i))))
  array)

(defun pwt! (array
             &key
               (wavelet :haar)
               (boundary-style :mirror)
               (cost *cost-func*))
  "Perform a best-basis PWT transform of an @c(array). Unlike DWT
which has a fixed basis of scaled and translated mother wavelet
functions and a scaling function, this transform picks the best basis,
minimizing the @c(cost) function over possible results. The meaning of
@c(wavelet) and @c(boundary-style) arguments is the same as for DWT.
See the documentation for @c(dwt!) for information.

Two values are returned: the first is a transformed array and the
second is a basis key object which must be passed to @c(pwt-inverse!)
or @c(pwt-inverse) to invert the transform.

This function modifies its first argument."
  (with-lifting-scheme (wavelet boundary-style)
    (let* ((*cost-func* cost)
           (tree (build-tree array)))
      (values
       (concatenate-into array (collect-preferred tree))
       (get-basis-key tree)))))

(defun pwt (array
             &key
               (wavelet :haar)
               (boundary-style :mirror)
               (cost *cost-func*))
  "This function is a non-destructive version of @c(pwt!)."
  (declare (type (sa-sb 32) array))
  (pwt! (copy-seq array)
        :wavelet wavelet
        :boundary-style boundary-style
        :cost cost))

(defun pwt-inverse! (array basis-key
                     &key
                       (wavelet :haar)
                       (boundary-style :mirror))
  "Inverse the PWT. The @c(array) and @c(basis-key) must match the
values returned by @c(pwt!) or @c(pwt). @c(wavelet) and
@c(boundary-style) must also match their counterparts used for the
forward transform."
  (declare (type (sa-sb 32) array))
  (labels ((pwt-inverse% (basis-key start end)
             (declare (type alex:non-negative-fixnum start end))
             (if basis-key
                 (let ((half (/ (+ end start) 2)))
                   (declare (type alex:non-negative-fixnum half))
                   (pwt-inverse% (first basis-key) start half)
                   (pwt-inverse% (second basis-key) half end)
                   (phase-mix array
                              :start start
                              :end end)
                   (funcall *lifting-func* array
                            :start start
                            :end end))
                 array)))
    (with-lifting-scheme (wavelet boundary-style :inverse t)
      (pwt-inverse% basis-key 0 (length array)))))

(defun pwt-inverse (array basis-key
                    &key
                      (wavelet :haar)
                      (boundary-style :mirror))
  "This is a non-destructive version of @c(pwt-inverse!)."
  (declare (type (sa-sb 32) array))
  (pwt-inverse! (copy-seq array) basis-key
                :wavelet wavelet
                :boundary-style boundary-style))

(defun basis-key=>bit-vector (basis-key)
  "Transform a basis key returned from @c(pwt) or @c(pwt!) to a bit
vector form."
  (declare (type list basis-key))
  (if basis-key
      (concatenate 'simple-bit-vector
                   #*1
                   (basis-key=>bit-vector (first basis-key))
                   (basis-key=>bit-vector (second basis-key)))
      #*0))

(defun bit-vector=>basis-key (vector)
  "Transform a bit vector to a basis key."
  (declare (type simple-bit-vector vector))
  (let ((first (aref vector 0))
        (rest (subseq vector 1)))
    (if (zerop first) (values nil rest)
        (multiple-value-bind (part1 rest)
            (bit-vector=>basis-key rest)
          (multiple-value-bind (part2 rest)
              (bit-vector=>basis-key rest)
            (values (list part1 part2)
                    rest))))))
