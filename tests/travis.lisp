(defun do-all()
  (ql:quickload :cl-wavelets/tests)
  (uiop:quit
   (if (uiop:call-function "cl-wavelets-tests:run-tests")
       0 1)))

(do-all)
