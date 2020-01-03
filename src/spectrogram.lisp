(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

(defvar *spectrogram-tmp* nil
  "Temporary array for spectrogram functions")

;; Full-depth PWT with rearranging subbands in natural frequency order
;; to calculate a spectrogram of a signal
(defun pwt-spectrogram (array &key
                                (start 0)
                                (end (length array))
                                inverse)
  (declare (type (sa-sb 32) array)
           (type non-negative-fixnum start end)
           (type boolean inverse))
  (if (< (- end start) 2)
      array
      (let ((half (/ (+ start end) 2)))
        (declare (type non-negative-fixnum half))
        (phase-split
         (funcall *lifting-func*
                  array
                  :start start
                  :end   end)
         :start   start
         :end     end
         :inverse inverse
         :tmp     *spectrogram-tmp*)
        (pwt-spectrogram array
                         :start start
                         :end   half)
        (pwt-spectrogram array
                         :start   half
                         :end     end
                         :inverse t)
        array)))

(defun spectrogram! (array &key (wavelet :haar) (boundary-style :zero))
  "Calculate spectrogram of a signal using PWT. WAVELET and
BOUNDARY-STYLE argument have the same meaning as for DWT! function"
  (declare (type (sa-sb 32) array))
  (let ((*aref-func* (get-boundary-style-lifting boundary-style))
        (*lifting-func* (get-forward-transform wavelet))
        (*spectrogram-tmp* (make-array (/ (check-length (length array)) 2)
                                       :element-type '(signed-byte 32))))
    (pwt-spectrogram array)))

(defun spectrogram (array &key (wavelet :haar) (boundary-style :zero))
  "Calculate spectrogram of a signal using PWT. This is a
non-destructive version on SPECTROGRAM!"
  (declare (type (sa-sb 32) array))
  (spectrogram! (copy-seq array)
                :wavelet        wavelet
                :boundary-style boundary-style))
