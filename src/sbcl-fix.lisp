(in-package :cl-wavelets)

;; This file helps to fix build on old sbcl versions prior to 1.5.0

;; The code below is taken from file code/target-misc.lisp in sbcl
;; distribution.
(defun split-version-string (string)
  (loop with subversion and start = 0
        with end = (length string)
        when (setf (values subversion start)
                   (parse-integer string :start start :junk-allowed t))
        collect it
        while (and subversion
                   (< start end)
                   (char= (char string start) #\.))
        do (incf start)))

(defun version>= (x y)
  (unless (or x y)
    (return-from version>= t))
  (let ((head-x (or (first x) 0))
        (head-y (or (first y) 0)))
    (or (> head-x head-y)
        (and (= head-x head-y)
             (version>= (rest x) (rest y))))))
;; The code above is taken from file code/target-misc.lisp in sbcl
;; distribution.

(defun parse-implementation (string)
  (let ((delimiter-pos (position #\- string)))
    (if delimiter-pos
        (values (subseq string 0 delimiter-pos)
                (subseq string (1+ delimiter-pos)))
        (error "Malformed implementation string"))))

(defun current-version<= (version-str)
  (let ((current-version (split-version-string (lisp-implementation-version)))
        (minimal-version (split-version-string version-str)))
    (version>= minimal-version current-version)))

(defun read-if-good-implementation (stream subchar arg)
  (declare (ignore subchar arg))
  (multiple-value-bind (implementation version)
      (parse-implementation (read stream))
    (if (or (current-version<= version)
            (string/= (lisp-implementation-type) implementation))
        (read stream)))
  (values))

(set-dispatch-macro-character #\# #\l #'read-if-good-implementation)
