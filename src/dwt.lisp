(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defun clamp-steps (steps max-steps)
  (declare (type fixnum steps max-steps))
  (min max-steps (max 0 steps)))

(defun dwt! (array &key
                     (wavelet        :haar)
                     (boundary-style :mirror)
                     (steps          0))
  "Perform in-place DWT transform on array of integer samples. Meaning
of key arguments:
@begin(enum)
  @item(@c(wavelet) Wavelet to use. Consult
        @ref[id=wavelets](wavelets) section of this manual to know
        possible values of this argument.)
  @item(@c(boundary-style) Determines how the signal is extended
        beyond array boundaries. Can be @c(:zero) or
        @c(:mirror). Usually @c(:mirror) gives better results, but
        is a little slower.)
  @item(@c(steps) Specify the number of filtering and downsampling
        steps taken to perform the transform. This value can be
        negative or zero. Zero means the full DWT transform and a
        negative value means @i(maximal dwt steps) - @c((abs steps)).)
@end(enum)"
  (declare (type (sa-sb 32) array)
           (type fixnum steps))
  (with-lifting-scheme (wavelet boundary-style)
    (let* ((len (check-power-of-2 (length array)))
           (max-steps (1- (integer-length len)))
           (dwt-steps (clamp-steps
                       (if (> steps 0) steps (+ max-steps steps))
                       max-steps))
           (tmp (make-tmp-array len)))
      (declare (type alex:non-negative-fixnum max-steps dwt-steps len))
      (loop for i below dwt-steps do
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
           (type fixnum steps))
  (with-lifting-scheme (wavelet boundary-style :inverse t)
    (let* ((len (check-power-of-2 (length array)))
           (max-steps (1- (integer-length len)))
           (dwt-steps (clamp-steps
                       (if (> steps 0) steps (+ max-steps steps))
                       max-steps))
           (tmp (make-tmp-array len)))
      (declare (type alex:non-negative-fixnum max-steps dwt-steps len))
      (loop for i from (- max-steps dwt-steps) below max-steps do
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
