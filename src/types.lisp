(in-package :cl-wavelets)

(deftype sa-sb (bits) `(simple-array (signed-byte ,bits)))
(deftype sb (bits) `(signed-byte ,bits))

(deftype aref-function ()
  '(function
    ((sa-sb 32) fixnum &key
     (:start alex:non-negative-fixnum)
     (:end   alex:non-negative-fixnum))
    (values (sb 32) &optional)))

(deftype lifting-function ()
  '(function
    ((sa-sb 32) &key
     (:start alex:non-negative-fixnum)
     (:end   alex:non-negative-fixnum))
    (values (sa-sb 32) &optional)))
