(defsystem :cl-wavelets
    :name :cl-wavelets
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :description "Wavelet transform library"
    :license "2-clause BSD"
    :serial t
    :components ((:file "src/package")
                 (:file "src/types")
                 (:file "src/sbcl-fix")
                 (:file "src/aref")
                 (:file "src/lifting")
                 (:file "src/general")
                 (:file "src/dwt")
                 (:file "src/freqdomain"))
    :in-order-to ((test-op (load-op "cl-wavelets/tests")))
    :perform (test-op (op system)
                      (declare (ignore op system))
                      (funcall
                       (symbol-function
                        (intern (symbol-name '#:run-tests)
                                (find-package :cl-wavelets-tests))))))

(defsystem :cl-wavelets/tests
    :name :cl-wavelets/tests
    :version "0.1"
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :components ((:file "tests/package")
                 (:file "tests/tests" :depends-on ("tests/package")))
    :depends-on (:cl-wavelets :fiveam))
