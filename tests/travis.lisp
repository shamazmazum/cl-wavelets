(defun do-all()
  (asdf:load-system :cl-wavelets/tests)
  (sb-ext:exit
   :code
   (if (funcall
        (intern (symbol-name :run-tests)
                (find-package :cl-wavelets-tests)))
        0 1)))

(do-all)
