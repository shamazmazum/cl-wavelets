(in-package :cl-wavelets-examples)

(defparameter *block-length* 512)

(defstruct image-size
  w h)

(defun get-color (x)
  "Get RGB color corresponding to integer value x in range 0 <= x < 256"
  (values x x x))

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
                                 :wavelet :cdf-2-2
                                 :boundary-style :mirror)))

    ;; Kinda normalize
    (let ((biggest (reduce #'max (map-into freq #'abs freq))))
      (if (not (zerop biggest))
          (map-into freq (lambda (x) (floor (* x 255) biggest)) freq)))

    (loop
       for i from (* 3 row (image-size-w size)) by 3
       for col below (image-size-w size)
       for x = (aref freq (floor (* *block-length* col)
                                 (image-size-w size)))
       do
         (multiple-value-bind (r g b)
             (get-color x)
           (setf (aref output (+ i 0)) r
                 (aref output (+ i 1)) g
                 (aref output (+ i 2)) b))))
  output)

(defun read-wav (name)
  "Read an audio file in wav format with the name NAME and return a
list of channel data and a format subchunk."
  (with-open-file (in name :element-type '(unsigned-byte 8))
    (let* ((reader (open-wav in))
           (subchunks (read-wav-header reader))
           (format (car subchunks)))
      (reader-position-to-audio-data reader subchunks)
      (values
       (read-wav-data reader
                      format
                      (samples-num subchunks)
                      :decompose t)
       format))))

(defun spectrogram (filename-wav filename-jpg
                    &key (w 800) (h 600))
  "Build a spectrogram for the first channel of an audio file in WAV
format with the name @c(filename-wav). The spectrogram is written as a
jpeg image with the name @c(filename-jpg). @c(w) and @c(h) parameters
define dimensions of the image.

PWT with CDF (2,2) wavelet is used in this function."
  (let ((input (first (read-wav filename-wav)))
        (output (make-array (* 3 w h) :element-type '(unsigned-byte 8)))
        (size (make-image-size :w w :h h)))
    (loop for row below h do
         (fill-row input output row size))
    (encode-image filename-jpg output 3 h w)))
