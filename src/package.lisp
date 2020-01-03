(defpackage cl-wavelets
  (:use #:cl)
  (:nicknames #:wavelets)
  (:export #:get-wavelets
           #:dwt
           #:dwt-inverse
           #:dwt!
           #:dwt-inverse!
           #:frequency-domain
           #:frequency-domain!))
