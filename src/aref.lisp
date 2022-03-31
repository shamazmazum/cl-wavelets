(in-package :cl-wavelets)
(declaim (optimize (speed 3))
         (ftype aref-function
                aref-zero aref-periodic
                aref-mirror-1 aref-mirror-2
                aref-extended)
         (type function *aref-func*))

(defun aref-zero (array idx &key (start 0) (end (length array)))
  "Return zero for out-of-bounds array reads. Can be used for every
wavelet and every DWT/PWT algorithm"
  (declare (type (sa-sb 32) array)
           (ignore idx start end))
  0)

(defun aref-periodic (array idx &key (start 0) (end (length array)))
  "Create an infinite sequence from an array by making the content of
an array periodic. Can be used with two-filters algorithm and all
wavelets."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type alex:non-negative-fixnum start end))
  ;; While filtering, the following assertion always holds
  (assert (>= idx start))
  (aref array (+ start
                 (rem (- idx start)
                      (- end start)))))

(defun aref-mirror-1 (array idx &key (start 0) (end (length array)))
  "'Mirror' content of an array from its borders mapping even indices
to even indices and odd indices to odd indices, so array[-1] ==
array[1] and array[len] == array[len-2].

This function can be used to create a continous infinite signal from a
signal with compact support in time domain for using in lifting scheme
or in two-filters algorithm with symmetric filters with odd number of
coefficients"
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type alex:non-negative-fixnum start end))
  (let* ((length (- end start))
         (fit-idx (rem (abs (- idx start))
                       (ash (1- length) 1))))
    (declare (type alex:non-negative-fixnum length fit-idx))
    (aref array
          (+ start
             (the alex:non-negative-fixnum
                  (if (< fit-idx length) fit-idx
                      (- (ash (1- length) 1) fit-idx)))))))

;; FIXME: Clearly will not work if start ~= 0
(defun aref-mirror-2 (array idx &key (start 0) (end (length array)))
  "'Mirror' content of an array from its borders, 'doubling' the
elements at the borders, so array[-1] == array[0] and array[len] ==
array[len-1].

This function can be used to create a continous infinite signal for
the wavelet transform using symmetric wavelets with even number of
coefficients and two-filters algorithm."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type alex:non-negative-fixnum start end))
  (let* ((length (- end start))
         (pos-idx (if (< idx 0) (abs (1+ idx)) idx))
         (fit-idx (rem pos-idx (ash length 1))))
    (declare (type alex:non-negative-fixnum
                   length pos-idx fit-idx))
    (aref array
          (if (< fit-idx length) fit-idx
              (- (ash length 1) 1 fit-idx)))))

(defparameter *aref-func* #'aref-zero
  "Current aref style")

(defun get-boundary-style-lifting (symbol)
  (ecase symbol
    (:zero   #'aref-zero)
    (:mirror #'aref-mirror-1)))

(defun aref-extended (array idx &key (start 0) (end (length array)))
  "This function works just like aref, but allows for idx to be any
integer number, not just one for which start <= idx < end holds. The
way how this function extends array out of its bounds depends on value
of *AREF-FUNC*."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type alex:non-negative-fixnum start end))
  (if (and (>= idx start)
           (<  idx end))
      (aref array idx)
      (funcall *aref-func*
               array idx
               :start start
               :end   end)))
