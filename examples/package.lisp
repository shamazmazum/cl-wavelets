(defpackage cl-wavelets-spectrogram
  (:use #:cl
        #:cl-jpeg
        #:wavelets)
  (:local-nicknames (:wav :easy-audio.wav))
  (:nicknames :wavelets-spectrogram)
  (:export #:spectrogram
           #:*block-length*
           #:*wavelet*))
