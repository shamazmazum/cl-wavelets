(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defun dwt! (array &key (wavelet :haar) (boundary-style :mirror))
  "Perform in-place DWT transform on array of integer samples. @c(wavelet)
argument can one of @c(:haar), @c(:cdf-2-2), @c(:cdf-3-1) or
@c(:cdf-4-2) (numbers after @c(cdf) part mean number of vanishing
moments for analysis and synthesis filters). @c(boundary-style) can be
@c(:zero) or @c(:mirror) and determines how the signal is extended
beyond array boundaries. Usually @c(:mirror) gives better results, but
is a little slower."
  (declare (type (sa-sb 32) array))
  (with-lifting-scheme (wavelet boundary-style)
    (let* ((len (check-power-of-2 (length array)))
           (times (1- (integer-length len)))
           (tmp (make-array (/ len 2) :element-type '(signed-byte 32))))
      (declare (type non-negative-fixnum len))
      (loop for i below times do
           (funcall *lifting-func* array :end (ash len (- i)))
           (phase-split array :tmp tmp :end (ash len (- i)))))
  array))

(defun dwt-inverse! (array &key (wavelet :haar) (boundary-style :mirror))
  "Perform in-place inversion of DWT transform. The @c(wavelet) and
@c(boundary-style) arguments must be the same as for the corresponding
call to DWT! function"
  (declare (type (sa-sb 32) array))
  (with-lifting-scheme (wavelet boundary-style :inverse t)
    (let* ((len (check-power-of-2 (length array)))
           (times (1- (integer-length len)))
           (tmp (make-tmp-array len)))
      (declare (type non-negative-fixnum len))
      (loop for i below times do
           (phase-mix array :tmp tmp :end (ash 2 i))
           (funcall *lifting-func* array :end (ash 2 i))))
    array))

(defun dwt (array &key (wavelet :haar) (boundary-style :mirror))
  "This function is a non-destructive equivalent of @c(dwt!) function"
  (declare (type (sa-sb 32) array))
  (dwt! (copy-seq array)
        :wavelet wavelet
        :boundary-style boundary-style))

(defun dwt-inverse (array &key (wavelet :haar) (boundary-style :mirror))
  "This function is a non-destructive equivalent of @c(dwt-inverse!)
function"
  (declare (type (sa-sb 32) array))
  (dwt-inverse! (copy-seq array)
                :wavelet wavelet
                :boundary-style boundary-style))
