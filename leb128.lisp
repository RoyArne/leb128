
(in-package #:cl-user)

;;;; Implements encoding and decoding integers in the signed and
;;;; unsigned LEB-128 format as described in the wikipedia article
;;;; 'https://en.wikipedia.org/wiki/LEB128'.

(defpackage #:leb128
  (:use #:cl)
  (:export #:unsigned-length
	   #:encode-unsigned
	   #:decode-unsigned

	   #:signed-length
	   #:encode-signed
	   #:encode-unsigned))

(in-package #:leb128)


(defun unsigned-length (integer)
  "Return the length of INTEGER in the unsigned LEB-128 format."
  (declare (type integer integer))
  (max 1 (ceiling (integer-length integer) 7)))

(defun signed-length (integer)
  "Return the length of INTEGER in the signed LEB-128 format."
  (declare (type integer integer))
  (unsigned-length (ash (if (minusp integer)
			    (lognot integer)
			    integer)
			1)))

(defun make-byte-vector (length)
  "Return a (unsigned-byte 8) vector of the given LENGTH."
  (declare (type integer length))
  (make-array length :element-type '(unsigned-byte 8) :initial-element 0))

(defun write-zero-bytes (count stream)
  "Write COUNT number of zero bytes to STREAM."
  (declare (type fixnum count)
	   (type stream stream))
  (loop repeat count do (write-byte 0 stream))
  (values))

(defun encode-low-7-bits (integer end-p)
  "Return a byte with its low 7 bits set to the low 7 bits of INTEGER,
and its eight bit set to one unless END-P is true."
  (declare (type integer integer)
	   (type t end-p))
  (if end-p
      (ldb (byte 7 0) integer)
      (logior (ldb (byte 7 0) integer) #x80)))

(defun emit-byte (integer output index end-p)
  "Return INTEGER shifted 7 bits to the right. Encode the low 7 bits
of INTEGER into OUTPUT, starting at INDEX if OUTPUT is a vector.

If END-P is true then this is the last byte of INTEGER and will be
encoded accordingly."
  (declare (type integer integer)
	   (type (or stream (vector (unsigned-byte 8))) output)
	   (type fixnum index)
	   (type t end-p))
  (typecase output
    (vector (setf (aref output index) (encode-low-7-bits integer end-p)))
    (stream (write-byte (encode-low-7-bits integer end-p) output)))
  (ash integer -7))

(defun encode-unsigned (integer &optional output (start 0))
  "Encode INTEGER in the unsigned LEB128 format.

If OUTPUT is a stream then write the encoded INTEGER to it, preceding
the encoding with START number of zero bytes. Return OUTPUT and the
number of bytes written.

If OUTPUT is NIL then create a (vector (unsigned-byte 8)) and encode
INTEGER into it. If START is given then precede the encoding with
START number of zero bytes. Return the vector, and its length.

If OUTPUT is a (vector (unsigned-byte 8)) then encode INTEGER into it,
beginning at START. Return OUTPUT and the end index of the encoding."
  (declare (type integer integer)
	   (type (or null stream (vector (unsigned-byte 8))) output)
	   (type fixnum start))
  (loop
     with end = (+ start (unsigned-length integer))
     with end-i = (1- end)
     with out = (or output (make-byte-vector end))
     initially (when (streamp out) (write-zero-bytes start out))
     for i from start below end
     for n = (emit-byte integer out i (= i end-i)) then (emit-byte n out i (= i end-i))
     finally (return (values out i))))

(defun encode-signed (integer &optional output (start 0))
  "Encode INTEGER in the signed LEB128 format.

If OUTPUT is a stream then write the encoded INTEGER to it, preceding
the encoding with START number of zero bytes. Return T and the number
of bytes written.

If OUTPUT is NIL then create a (vector (unsigned-byte 8)) and encode
INTEGER into it. If START is given then precede the encoding with
START number of zero bytes. Return the vector, and its length.

If OUTPUT is a (vector (unsigned-byte 8)) then encode INTEGER into it,
beginning at START. Return OUTPUT and the end index of the encoding."
  (declare (type integer integer)
	   (type (or null stream (vector (unsigned-byte 8))) output)
	   (type fixnum start))
  (loop
     with end = (+ start (signed-length integer))
     with end-i = (1- end)
     with out = (or output (make-byte-vector end))
     for i from start below end
     for n = (emit-byte integer out i (= i end-i)) then (emit-byte n out i (= i end-i))
     finally (return (values out i))))

(defun read-zero-bytes (count stream)
  "Read COUNT number of zero bytes from STREAM."
  (declare (type fixnum count)
	   (type stream stream))

  (loop repeat count do (read-byte stream))
  (values))

(defun consume-byte (input index)
  "Return the next byte from INPUT."
  (declare (type (or stream (vector (unsigned-byte 8))) input)
	   (type fixnum index))
  (typecase input
    (vector (aref input index))
    (stream (read-byte input))))

(defun decode-byte (integer byte shift)
  "Return INTEGER with high bits starting at shift containing the low
7 bits of BYTE."
  (declare (type integer integer shift)
	   (type (unsigned-byte 8) byte))
  (logior integer (ash (ldb (byte 7 0) byte) shift)))

(defun decode-unsigned (input &OPTIONAL (start 0))
  "Return an integer decoded from INPUT beginning at START,
and the end index of the encoded value.

The integer is decoded from the unsigned LEB-128 format.

INPUT may be a stream or a (vector (unsigned-byte 8)).

If INPUT is a stream then START number of bytes is consumed and
discarded before the integer is decoded. The end index returned will
be the number of bytes consumed in total."
  (declare (type (or stream (vector (unsigned-byte 8))) input)
	   (type fixnum start))
  (loop
     initially (when (streamp input) (read-zero-bytes start input))
     for i = start then (1+ i)
     for shift = 0 then (+ shift 7)
     for byte = (consume-byte input i)
     for n = (decode-byte 0 byte shift) then (decode-byte n byte shift)
     until (zerop (logand byte #x80))
     finally (return (values n (1+ i)))))

(defun decode-signed (input &optional (start 0))
  "Return an integer decoded from INPUT beginning at START,
and the end index of the encoded value.

The integer is decoded from the signed LEB-128 format.

INPUT may be a stream or a (vector (unsigned-byte 8)).

If INPUT is a stream then START number of bytes is consumed and
discarded before the integer is decoded. The end index returned will
be the number of bytes consumed in total."
  (declare (type (or stream (vector (unsigned-byte 8))) input)
	   (type fixnum start))
  (loop
     initially (when (streamp input) (read-zero-bytes start input))
     for i = start then (1+ i)
     for shift = 0 then (+ shift 7)
     for byte = (consume-byte input i)
     for integer = (decode-byte 0 byte shift) then (decode-byte integer byte shift)
     until (zerop (logand byte #x80))
     finally (return (values (if (plusp (logand byte #x40))
				 (logior integer (ash -1 (+ shift 7)))
				 integer)
			     (1+ i)))))
