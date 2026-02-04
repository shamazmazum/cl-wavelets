(defun do-all()
  (handler-case
      (asdf:load-system :cl-wavelets/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "cl-wavelets-tests:run-tests")
       0 1)))

(do-all)
