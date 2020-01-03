(in-package :cl-wavelets)
(declaim (optimize (speed 3))
         (ftype aref-function
                aref-zero aref-periodic
                aref-mirror-1 aref-mirror-2)
         #l"SBCL-1.5.0"
         (type aref-function *aref-func*))

(defun aref-zero (array idx &key (start 0) (end (length array)))
  "Create an infinite sequence from an array simply returning zeros if
an index `idx` causes out-of-bound access. Can be used with all
wavelets and all transform algorithms."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type non-negative-fixnum start end))
  (if (and (>= idx start)
           (< idx end))
      (aref array idx) 0))

(defun aref-periodic (array idx &key (start 0) (end (length array)))
  "Create an infinite sequence from an array by making the content of
an array periodic. Can be used with two-filters scheme and all
wavelets."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type non-negative-fixnum start end))
  (aref array (+ start (rem idx (- end start)))))

;; FIXME: Clearly will not work if start ~= 0
(defun aref-mirror-1 (array idx &key (start 0) (end (length array)))
  "'Mirror' content of an array from its borders mapping even indices
to even indices and odd indices to odd indices, so array[-1] ==
array[1] and array[len] == array[len-2].

This function can be used to create a continous infinite signal from a
signal with compact support in time domain for using in lifting scheme
or in two-filters scheme with symmetric filters with odd number of
coefficients"
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type non-negative-fixnum start end))
  (aref array
        ;; This if is here only for speed, the "else"
        ;; expression works fine in the both cases
        (if (and (>= idx start)
                 (< idx end))
            idx
            (let* ((length (- end start))
                   (fit-idx (abs (rem idx (ash (1- length) 1)))))
              (declare (type non-negative-fixnum length fit-idx))
              (if (< fit-idx length) fit-idx
                  (- (ash (1- length) 1) fit-idx))))))

;; FIXME: Clearly will not work if start ~= 0
(defun aref-mirror-2 (array idx &key (start 0) (end (length array)))
  "'Mirror' content of an array from its borders, 'doubling' the
elements at the borders, so array[-1] == array[0] and array[len] ==
array[len-1].

This function can be used to create a continous infinite signal for
the wavelet transform using symmetric wavelets with even number of
coefficients and two-filters scheme."
  (declare (type (sa-sb 32) array)
           (type fixnum idx)
           (type non-negative-fixnum start end))
  (aref array
        ;; This if is here only for speed, the "else"
        ;; expression works fine in the both cases
        (if (and (>= idx start)
                 (< idx end))
            idx
            (let* ((length (- end start))
                   (pos-idx (if (< idx 0) (abs (1+ idx)) idx))
                   (fit-idx (rem pos-idx (ash length 1))))
              (declare (type non-negative-fixnum
                             length pos-idx fit-idx))
              
              (if (< fit-idx length) fit-idx
                  (- (ash length 1) 1 fit-idx))))))

(defparameter *aref-func* #'aref-zero
  "Current aref style")

(defun get-boundary-style-lifting (symbol)
  (ecase symbol
    (:zero   #'aref-zero)
    (:mirror #'aref-mirror-1)))
