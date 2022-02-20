(defsystem :cl-wavelets
    :name :cl-wavelets
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Wavelet transform library"
    :license "2-clause BSD"
    :serial t
    :pathname "src/"
    :components ((:file "package")
                 (:file "types")
                 (:file "aref")
                 (:file "lifting")
                 (:file "phases")
                 (:file "dwt")
                 (:file "freqdomain")
                 (:file "pwt"))
    :in-order-to ((test-op (load-op "cl-wavelets/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :cl-wavelets-tests '#:run-tests)))

(defsystem :cl-wavelets/tests
    :name :cl-wavelets/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "tests/"
    :components ((:file "package")
                 (:file "tests" :depends-on ("package")))
    :depends-on (:cl-wavelets :fiveam))

(defsystem :cl-wavelets/examples
    :name :cl-wavelets/examples
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :pathname "examples/"
    :components ((:file "package")
                 (:file "spectrogram" :depends-on ("package")))
    :depends-on (:cl-wavelets :easy-audio :cl-jpeg))
