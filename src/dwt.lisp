(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defun dwt! (array &key
                     (wavelet        :haar)
                     (boundary-style :mirror)
                     (steps          0))
  "Perform in-place DWT transform on array of integer samples. Meaning
of key arguments:
@begin(enum)
  @item(@c(wavelet) Wavelet to use. Can one of @c(:haar),
        @c(:cdf-2-2), @c(:cdf-3-1) or @c(:cdf-4-2) (numbers after
        @c(cdf) part mean number of vanishing moments for analysis and
        synthesis filters).)
  @item(@c(boundary-style) Determines how the signal is extended
        beyond array boundaries. Can be @c(:zero) or
        @c(:mirror). Usually @c(:mirror) gives better results, but
        is a little slower.)
  @item(@c(steps) Specify the number of filtering and downsampling
        steps taken to perform the transform. A special value @c(0)
        means the full transform.)
@end(enum)"
  (declare (type (sa-sb 32) array)
           (type non-negative-fixnum steps))
  (with-lifting-scheme (wavelet boundary-style)
    (let* ((len (check-power-of-2 (length array)))
           (max-steps (1- (integer-length len)))
           (steps (if (zerop steps) max-steps (min steps max-steps)))
           (tmp (make-array (/ len 2) :element-type '(signed-byte 32))))
      (declare (type non-negative-fixnum max-steps len))
      (loop for i below steps do
           (funcall *lifting-func* array :end (ash len (- i)))
           (phase-split array :tmp tmp :end (ash len (- i)))))
  array))

(defun dwt-inverse! (array &key
                             (wavelet        :haar)
                             (boundary-style :mirror)
                             (steps          0))
  "Perform in-place inversion of DWT transform. The @c(wavelet),
@c(boundary-style) and @c(steps) arguments must be the same as for the
corresponding call to DWT! function"
  (declare (type (sa-sb 32) array)
           (type non-negative-fixnum steps))
  (with-lifting-scheme (wavelet boundary-style :inverse t)
    (let* ((len (check-power-of-2 (length array)))
           (max-steps (1- (integer-length len)))
           (steps (if (zerop steps) max-steps (min steps max-steps)))
           (tmp (make-tmp-array len)))
      (declare (type non-negative-fixnum len))
      (loop for i from (- max-steps steps) below max-steps do
           (phase-mix array :tmp tmp :end (ash 2 i))
           (funcall *lifting-func* array :end (ash 2 i))))
    array))

(defun dwt (array &key
                    (wavelet        :haar)
                    (boundary-style :mirror)
                    (steps          0))
  "This function is a non-destructive equivalent of @c(dwt!) function"
  (declare (type (sa-sb 32) array))
  (dwt! (copy-seq array)
        :wavelet        wavelet
        :boundary-style boundary-style
        :steps          steps))

(defun dwt-inverse (array &key
                            (wavelet        :haar)
                            (boundary-style :mirror)
                            (steps          0))
  "This function is a non-destructive equivalent of @c(dwt-inverse!)
function"
  (declare (type (sa-sb 32) array))
  (dwt-inverse! (copy-seq array)
                :wavelet        wavelet
                :boundary-style boundary-style
                :steps          steps))
