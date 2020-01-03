(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defun dwt! (array &key (wavelet :haar) (boundary-style :mirror))
  "Perform in-place DWT transform on array of integer samples. WAVELET
argument can one of :HAAR, :CDF-2-2 or :CDF-4-2 (numbers after 'CDF'
part mean number of vanishing moments for analysis and synthesis
filters). BOUNDARY-STYLE can be :ZERO or :MIRROR and determines how
the signal is extended beyond array boundaries. Usually :MIRROR gives
better results, but is a little slower."
  (declare (type (sa-sb 32) array))
  (let* ((*aref-func* (get-boundary-style-lifting boundary-style))
         (*lifting-func* (get-forward-transform wavelet))
         (len (check-length (length array)))
         (times (1- (integer-length len)))
         (tmp (make-array (/ len 2) :element-type '(signed-byte 32))))
    (declare (type non-negative-fixnum len))
    (loop for i below times do
         (funcall *lifting-func* array :end (ash len (- i)))
         (setq array (phase-split array :tmp tmp :end (ash len (- i))))))
  array)

(defun dwt-inverse! (array &key (wavelet :haar) (boundary-style :zero))
  "Perform in-place inversion of DWT transform. The WAVELET and
BOUNDARY-STYLE arguments must be the same as for the corresponding
call to DWT! function"
  (declare (type (sa-sb 32) array))
  (let* ((*aref-func* (get-boundary-style-lifting boundary-style))
         (*lifting-func* (get-inverse-transform wavelet))
         (len (check-length (length array)))
         (times (1- (integer-length len)))
         (tmp (make-array (/ len 2) :element-type '(signed-byte 32))))
    (declare (type non-negative-fixnum len))
    (loop for i below times do
         (setq array (phase-mix array :tmp tmp :end (ash 2 i)))
         (funcall *lifting-func* array :end (ash 2 i))))
  array)

(defun dwt (array &key (wavelet :haar) (boundary-style :zero))
  "This function is a non-destructive equivalent of DWT! function"
  (declare (type (sa-sb 32) array))
  (dwt! (copy-seq array)
        :wavelet wavelet
        :boundary-style boundary-style))

(defun dwt-inverse (array &key (wavelet :haar) (boundary-style :zero))
  "This function is a non-destructive equivalent of DWT-INVERSE!
function"
  (declare (type (sa-sb 32) array))
  (dwt-inverse! (copy-seq array)
                :wavelet wavelet
                :boundary-style boundary-style))
