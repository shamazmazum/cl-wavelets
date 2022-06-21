(in-package :cl-wavelets-spectrogram)

(defparameter *block-length* 512
  "The size of data chunks the input signal is divided to before PWT
is performed. Must be a power of 2.")
(defparameter *wavelet* :cdf-2-2
  "Wavelet used for spectrogram calculation. @b(NB): @c(:cdf-4-2) works
very bad, do not use it. @c(:cdf-2-2) is the best choice.")

(defparameter *ncolors* 1024)
(defparameter *color-table*
  (list
   (cons 0                     #(0   0   0))
   (cons (floor *ncolors* 4)   #(255 0   0))
   (cons (floor *ncolors* 3)   #(0   0   255))
   (cons (floor *ncolors* 1.5) #(0   255 255))
   (cons (1- *ncolors*)        #(255 255 255))))

(defun interpolate (x min max)
  (destructuring-bind (min-x . min-color) min
    (destructuring-bind (max-x . max-color) max
      (let ((color-diff (map 'vector #'- max-color min-color))
            (x-diff  (- max-x min-x))
            (x-start (- x min-x)))
        (map 'vector #'+ min-color
             (map 'vector
                  (lambda (color)
                    (floor (* (/ x-start x-diff) color)))
                  color-diff))))))

(defun get-color (x)
  "Get RGB color corresponding to integer value x in the range
0 <= x < *ncolors*"
  (let ((min-pos
         (position x *color-table*
                   :test #'>=
                   :key  #'car
                   :from-end t)))
    (if (= min-pos (1- (length *color-table*)))
        (cdr (nth min-pos *color-table*))
        (interpolate x
                     (nth min-pos *color-table*)
                     (nth (1+ min-pos) *color-table*)))))

(defstruct image-size
  w h)

(defun get-block (array start)
  "Get subsequence of ARRAY from position START with length
*BLOCK-LENGTH*, padding with zeros if the ARRAY ends."
  (let* ((length (length array))
         (end (min (+ start *block-length*) length))
         (block-length (- end start))
         (data-block (subseq array start end)))
    (if (= block-length *block-length*)
        data-block
        (make-array *block-length*
                    :element-type '(signed-byte 32)
                    :initial-contents
                    (concatenate 'list
                                 data-block
                                 (loop
                                    repeat (- *block-length* block-length)
                                    collect 0))))))

(defun get-block-for-row (array row size)
  "Get a chunk of data from an ARRAY corresponding to a row ROW of a
spectrogram with the size SIZE."
  (declare (type image-size size))
  (get-block array (floor (* (length array) row)
                          (image-size-h size))))

(defun fill-row (input output row size)
  "Fill the row of a spectrogram. The input signal is in array
INPUT, and the output image is in array OUTPUT which has element type
'(UNSIGNED-BYTE 8) and contains 3 elements per pixel."
  (declare (type image-size size))
  (let* ((datablock (get-block-for-row input row size))
         (freq (frequency-domain datablock
                                 :wavelet *wavelet*)))

    ;; Kinda normalize
    (let ((biggest (reduce #'max (map-into freq #'abs freq))))
      (if (not (zerop biggest))
          (map-into freq
                    (lambda (x)
                      (floor (* x (1- *ncolors*))
                             biggest))
                    freq)))

    (loop
       for i from (* 3 row (image-size-w size)) by 3
       for col below (image-size-w size)
       for x = (aref freq (floor (* *block-length* col)
                                 (image-size-w size)))
       for color = (get-color x)
       do
         (setf (aref output (+ i 0))
               (aref color 0)
               (aref output (+ i 1))
               (aref color 1)
               (aref output (+ i 2))
               (aref color 2))))
  output)

(defun read-wav (name)
  "Read an audio file in wav format with the name NAME and return a
list of channel data and a format subchunk."
  (with-open-file (in name :element-type '(unsigned-byte 8))
    (let* ((reader (wav:open-wav in))
           (subchunks (wav:read-wav-header reader))
           (format (car subchunks)))
      (wav:reader-position-to-audio-data reader subchunks)
      (values
       (wav:read-wav-data reader
                          format
                          (wav:samples-num subchunks)
                          :decompose t)
       format))))

(defun spectrogram (filename-wav
                    filename-jpg
                    &key
                      (w 600)
                      (h 800)
                      (block-length *block-length*)
                      (wavelet *wavelet*))
  "Build a spectrogram for the first channel of an audio file in WAV
format with the name @c(filename-wav). The spectrogram is written as a
jpeg image with the name @c(filename-jpg). @c(w) and @c(h) parameters
define dimensions of the image.

The spectrogram is built using @c(frequency-domain) function with data
block of length @c(block-length) each constructed from the input data.
Wavelet @c(wavelet) is used in the PWT algorithm."
  (let ((input (first (read-wav filename-wav)))
        (output (make-array (* 3 w h) :element-type '(unsigned-byte 8)))
        (size (make-image-size :w w :h h))
        (*block-length* block-length)
        (*wavelet* wavelet))
    (loop for row below h do
         (fill-row input output row size))
    (encode-image filename-jpg output 3 h w)))
