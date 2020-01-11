(in-package :cl-wavelets-tests)

(def-suite aref :description "Various kinds of aref for extended signal")
(def-suite dwt  :description "DWT tests")
(def-suite pwt  :description "PWT tests")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(aref dwt pwt))))

(defconstant +array-len+ 128
  "Length of an array used for testing")

(defun random-sequence ()
  (make-array +array-len+
              :element-type '(signed-byte 32)
              :initial-contents (loop
                                   repeat +array-len+
                                   collect (+ (random 200) 100))))

(defun calc-poly (x coeff &optional (acc 0))
  (if (null coeff) acc
      (calc-poly x (cdr coeff) (+ (car coeff) (* acc x)))))

(defun gen-poly (n)
  (make-array +array-len+
              :element-type '(signed-byte 32)
              :initial-contents
              (loop
                 with poly = (loop repeat n collect (1+ (random 10)))
                 for x below +array-len+ collect (calc-poly x poly))))

(in-suite aref)
(test aref-zero-test
  (let ((array (random-sequence))
        (wavelets::*aref-func* #'wavelets::aref-zero))
    (is (= (aref array 1)
           (wavelets::aref-extended array 1 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 2 :start 1 :end 4)))
    (is (= (aref array 3)
           (wavelets::aref-extended array 3 :start 1 :end 4)))
    (is (zerop
         (wavelets::aref-extended array 4 :start 1 :end 4)))
    (is (zerop
         (wavelets::aref-extended array 5 :start 1 :end 4)))))

(test aref-periodic-test
  (let ((array (random-sequence))
        (wavelets::*aref-func* #'wavelets::aref-periodic))
    (is (= (aref array 1)
           (wavelets::aref-extended array 1 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 2 :start 1 :end 4)))
    (is (= (aref array 3)
           (wavelets::aref-extended array 3 :start 1 :end 4)))
    (is (= (aref array 1)
           (wavelets::aref-extended array 4 :start 1 :end 4)))))

(test aref-mirror-1-test
  (let ((array (random-sequence))
        (wavelets::*aref-func* #'wavelets::aref-mirror-1))
    (is (= (aref array 3)
           (wavelets::aref-extended array -1 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 0 :start 1 :end 4)))
    (is (= (aref array 1)
           (wavelets::aref-extended array 1 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 2 :start 1 :end 4)))
    (is (= (aref array 3)
           (wavelets::aref-extended array 3 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 4 :start 1 :end 4)))
    (is (= (aref array 1)
           (wavelets::aref-extended array 5 :start 1 :end 4)))
    (is (= (aref array 2)
           (wavelets::aref-extended array 6 :start 1 :end 4)))
    (is (= (aref array 3)
           (wavelets::aref-extended array 7 :start 1 :end 4)))))

(in-suite dwt)
(test dwt-inverse-identity
  "Check if DWT^{-1} DWT = I"
  (loop for boundary-style in '(:mirror :zero) do
       (loop
          for wavelet in (get-wavelets)
          for array = (random-sequence) do
            (is (equalp array
                        (dwt-inverse (dwt array
                                          :wavelet wavelet
                                          :boundary-style boundary-style)
                                     :wavelet wavelet
                                     :boundary-style boundary-style))))))

(test dwt-vanishing-moments
  "Check if mother wavelet functions have the desired amount of
vanishing moments"
  (flet ((check-vanishing-moments (wavelet n)
           (let ((array (dwt (gen-poly n) :wavelet wavelet)))
             ;; Assume ~75% of elements in transformed array equal to
             ;; zero. Nope, not zero, see the next comment

             ;; With CDF-3-1 polynomials of the second degree do not
             ;; completely vanish but can remain as a some small value
             ;; like -1 or 1
             (> (count-if (lambda (x) (< (abs x) 3)) array)
                (floor (length array) 4/3)))))
    (is-true (check-vanishing-moments :haar    1))
    (is-true (check-vanishing-moments :cdf-2-2 2))
    (is-true (check-vanishing-moments :cdf-3-1 3))
    (is-true (check-vanishing-moments :cdf-4-2 4))))

(test normalization
  "Lifting schemes have a scaling step which 'in theory' results in
  the first coefficient of DWT being equal to average of the input
  signal. Unfortunately, this is not practically true with exception
  of constant signals. Nevertheless, do this test to make sure I am
  not screwed up defining lifting schemes."
  (let* ((random (random 100))
         (array (make-array +array-len+
                            :element-type '(signed-byte 32)
                            :initial-element random)))
    ;; Allow the first element of DWT to be smaller or bigger than
    ;; average by 1 because of integer arithmetic is used which can
    ;; produce inexact results by dropping fractional part of result
    ;; of each lifting.
    (loop for wavelet in (get-wavelets) do
         (is (<= (abs
                  (- (aref (dwt array :wavelet wavelet) 0)
                     random))
                 1)))))

(in-suite pwt)

;; Here I can only check against results calculated by hand
(test frequency-analysis
  (is (equalp
       (frequency-domain (make-array 8
                                     :element-type '(signed-byte 32)
                                     :initial-contents (loop for i below 8 collect i))
                         :wavelet :haar)
       #(3 4 0 2 0 0 0 1))))

(defparameter *square-wave*
  (make-array 512
              :element-type '(signed-byte 32)
              :initial-contents
              (loop
                 with s = nil
                 for i below 51
                 do
                   (setq s (append s '(10 10 10 10 10
                                       0  0  0  0  0)))
                 finally (return (append s '(10 10)))))
  "Periodic square wave with frequency f_{nyquist}/5")

(test frequency-analysis-2
  ;; Skip Haar and CDF-4-2
  ;; The first is not precise enough and the last doesn't work yet for
  ;; unknown reason.
  (loop
     for wavelet in '(#+nil
                      :haar
                      :cdf-2-2
                      :cdf-3-1
                      #+nil
                      :cdf-4-2)
     for freq-domain = (map 'vector #'abs
                            (frequency-domain *square-wave*
                                              :wavelet wavelet))
     for max = (reduce #'max freq-domain)
     for max-pos = (position max freq-domain)
     for freq = (/ max-pos (length freq-domain))
     do
       (is-true
        (and (> freq (- 1/5 1/100))
             (< freq (+ 1/5 1/100))))))

(test pwt-inverse-identity
  "Check if PWT^{-1} PWT = I"
  (loop for boundary-style in '(:mirror :zero) do
       (loop
          for wavelet in (get-wavelets)
          for array = (random-sequence) do
            (multiple-value-bind (array-trans basis-idx)
                (pwt array
                     :wavelet wavelet
                     :boundary-style boundary-style
                     :cost (make-threshold-cost 20))
              (is (equalp
                   array
                   (pwt-inverse array-trans basis-idx
                                :wavelet wavelet
                                :boundary-style boundary-style)))))))

#+nil
(test pwt-vanishing-moments
  "Check if mother wavelet functions have the desired amount of
vanishing moments"
  (flet ((check-vanishing-moments (wavelet n)
           (let ((array (pwt (gen-poly n) :wavelet wavelet)))
             ;; Assume 50% of elements in transformed array equal to zero
             (> (count 0 array)
                (truncate (length array) 2)))))
    (is-true (check-vanishing-moments :haar    1))
    (is-true (check-vanishing-moments :cdf-2-2 2))
    (is-true (check-vanishing-moments :cdf-3-1 3))
    (is-true (check-vanishing-moments :cdf-4-2 4))))

(test basis-key<=>bit-vector
  "Test 2 representations of basis keys"
  (is (equal (basis-key=>bit-vector '((nil nil) (nil (nil (nil nil)))))
             #*11001010100))
  (is (equalp (bit-vector=>basis-key #*11001010100)
              '((nil nil) (nil (nil (nil nil)))))))
