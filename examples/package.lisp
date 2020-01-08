(defpackage cl-wavelets-spectrogram
  (:use #:cl
        #:wav
        #:cl-jpeg
        #:wavelets)
  (:nicknames :wavelets-spectrogram)
  (:export #:spectrogram
           #:*block-length*
           #:*wavelet*))
