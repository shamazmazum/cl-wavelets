(in-package :cl-wavelets)

(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))
(deftype non-negative-fixnum () '(integer 0 #.most-positive-fixnum))
(deftype sa-sb (bits) `(simple-array (signed-byte ,bits)))
(deftype sb (bits) `(signed-byte ,bits))

(deftype aref-function ()
  '(function
    ((sa-sb 32) fixnum &key
     (:start non-negative-fixnum)
     (:end   non-negative-fixnum))
    (values (sb 32) &optional)))

(deftype lifting-function ()
  '(function
    ((sa-sb 32) &key
     (:start non-negative-fixnum)
     (:end   non-negative-fixnum))
    (values (sa-sb 32) &optional)))
