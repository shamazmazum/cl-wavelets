(in-package :cl-wavelets-tests)

(def-suite dwt :description "DWT tests")
(def-suite pwt :description "PWT tests")

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(dwt pwt))))

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
                 with poly = (loop repeat n collect (1+ (random 5)))
                 for x below +array-len+ collect (calc-poly x poly))))

(defun check-vanishing-moments (wavelet n)
  (let ((array (dwt (gen-poly n) :wavelet wavelet)))
    ;; Assume 75% of elements in transformed array equal to zero
    (> (count 0 array)
       (truncate (length array) 4/3))))

(in-suite dwt)
(test dwt-inverse-identity
  "Check if DWT^1 DWT = I"
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
(test vanishing-moments
  "Check if mother wavelet functions have the desired amount of
vanishing moments"
  (is (check-vanishing-moments :haar    1))
  (is (check-vanishing-moments :cdf-2-2 2))
  (is (check-vanishing-moments :cdf-4-2 4)))

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
    (loop for wavelet in (get-wavelets) do
         (is (= random (aref (dwt array :wavelet wavelet) 0))))))

(in-suite pwt)

;; Here I can only check against results calculated by hand
(test frequency-analysis
  (is (equalp
       (frequency-domain (make-array 8
                                     :element-type '(signed-byte 32)
                                     :initial-contents (loop for i below 8 collect i))
                         :wavelet :haar)
       #(3 4 0 2 0 0 0 1))))
