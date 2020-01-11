(in-package :cl-wavelets)
(declaim (optimize (speed 3)))

;; Full-depth PWT with rearranging subbands in natural frequency order
;; to calculate a signal in frequency domain
(defun pwt-freq (array &key
                         (start 0)
                         (end (length array))
                         tmp
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
         :tmp     tmp)
        (pwt-freq array
                  :start start
                  :end   half
                  :tmp   tmp)
        (pwt-freq array
                  :start   half
                  :end     end
                  :tmp     tmp
                  :inverse t)
        array)))

(defun frequency-domain! (array &key (wavelet :haar) (boundary-style :zero))
  "Translate a signal into frequency domain using PWT. @c(wavelet)
argument can one of @c(:haar), @c(:cdf-2-2) or @c(:cdf-4-2) (numbers
after @c(cdf) part mean number of vanishing moments for analysis and
synthesis filters). @c(boundary-style) can be @c(:zero) or @c(:mirror)
and determines how the signal is extended beyond array
boundaries. Usually @c(:mirror) gives better results, but is a little
slower. This function modifies the @c(array)."
  (declare (type (sa-sb 32) array))
  (with-lifting-scheme (wavelet boundary-style)
    (let ((tmp (make-tmp-array
                (check-power-of-2
                 (length array)))))
      (pwt-freq array :tmp tmp))))

(defun frequency-domain (array &key (wavelet :haar) (boundary-style :zero))
  "Translate a signal in frequency domain using PWT. This is a
non-destructive version of @c(frequency-domain!)."
  (declare (type (sa-sb 32) array))
  (frequency-domain! (copy-seq array)
                     :wavelet        wavelet
                     :boundary-style boundary-style))
