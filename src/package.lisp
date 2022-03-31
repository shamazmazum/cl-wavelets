(defpackage cl-wavelets
  (:use #:cl)
  (:nicknames #:wavelets)
  (:local-nicknames (:alex :alexandria)
                    (:sera :serapeum))
  (:export #:get-wavelets
           #:dwt
           #:dwt-inverse
           #:dwt!
           #:dwt-inverse!
           #:frequency-domain
           #:frequency-domain!
           #:make-threshold-cost
           #:*threshold*
           #:pwt
           #:pwt-inverse
           #:pwt!
           #:pwt-inverse!
           #:basis-key=>bit-vector
           #:bit-vector=>basis-key))
