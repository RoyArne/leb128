;;;; leb128/leb128.lisp
;;;;
(in-package :roy-arne.gangstad.leb128)


(defun unsigned-length (integer)
  "Return the length of INTEGER in the unsigned LEB-128 format."
  
  (declare (type integer integer))

  (max 1 (ceiling (integer-length integer) 7)))



(defun encode-unsigned (integer &optional output (start 0))
  "Encode INTEGER in the unsigned LEB128 format.

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

  (let* ((length (unsigned-length integer))
	 (end (+ start length)))
    
    (when (null output)
      (setf output (make-array end :element-type '(unsigned-byte 8) :initial-element 0)))

    (etypecase output
      (vector
       (loop
	  for i from start below end
	  for byte = (logand integer #x7F)
	  do (setf integer (ash integer -7))
	  do (setf (aref output i) (if (zerop integer)
				       byte
				       (logior byte #x80)))
	  finally (return (values output i))))
      (stream
       (loop
	  initially (when (plusp start)
		      (loop
			 for i from 1 upto start
			 do (write-byte 0 output)))
	  for i from start below end
	  for byte = (logand integer #x7F)
	  do (setf integer (ash integer -7))
	  do (write-byte (if (zerop integer)
			     byte
			     (logior byte #x80))
			 output)
			     
	  finally (return (values t i)))))))
	
      


(defun decode-unsigned (input &optional (start 0))
  "Return an integer decoded from INPUT beginning at START,
and the end index of the encoded value.

The integer is decoded from the unsigned LEB-128 format.

INPUT may be a stream or a (vector (unsigned-byte 8)).

If INPUT is a stream then START number of bytes is consumed and
discarded before the integer is decoded. The end index returned will
be the number of bytes consumed in total."

  (declare (type (or stream (vector (unsigned-byte 8))) input)
	   (type fixnum start))

  (let ((integer 0))
    (etypecase input
      (vector
       (loop
	  for byte = (aref input start)
	  for shift = 0 then (+ shift 7)
	  do (setf integer (logior integer (ash (logand byte #x7F) shift)))
	  do (incf start)
	  until (zerop (logand byte #x80))))
      (stream
       (loop
	  initially (when (plusp start)
		      (loop
			 for i from 1 upto start
			 do (read-byte input)))
	  for byte = (read-byte input)
	  for shift = 0 then (+ shift 7)
	  do (setf integer (logior integer (ash (logand byte #x7F) shift)))
	  do (incf start)
	  until (zerop (logand byte #x80)))))
    (values integer start)))



(defun signed-length (integer)
  "Return the length of INTEGER in the signed LEB-128 format."
  
  (declare (type integer integer))

  (unsigned-length (ash (if (minusp integer)
			    (lognot integer)
			    integer)
			1)))



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

  (let* ((length (signed-length integer))
	 (end (+ start length)))
    
    (when (null output)
      (setf output (make-array end :element-type '(unsigned-byte 8) :initial-element 0)))

    (etypecase output
      (vector
       (loop
	  for i from start below end
	  for byte = (logand integer #x7F)
	  do (setf integer (ash integer -7))
	  do (setf (aref output i) (if (= i (1- end))
				       byte
				       (logior byte #x80)))
	  finally (return (values output i))))
      (stream
       (loop
	  initially (when (plusp start)
		      (loop
			 for i from 1 upto start
			 do (write-byte 0 output)))
	  for i from start below end
	  for byte = (logand integer #x7F)
	  do (setf integer (ash integer -7))
	  do (write-byte (if (= i (1- end))
			     byte
			     (logior byte #x80))
			 output)
	  finally (return (values t i)))))))



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
  
  (let ((integer 0)
	(shift 0))
    (etypecase input
      (vector
       (loop
	  for byte = (aref input start)
	  do (setf integer (logior integer (ash (logand byte #x7F) shift)))
	  do (incf shift 7)
	  do (incf start 1)
	  until (zerop (logand byte #x80))
	  finally (setf integer (if (plusp (logand byte #x40))
				    (logior integer (ash -1 shift))
				    integer))))
      (stream
       (loop
	  initially (when (plusp start)
		      (loop
			 for i from 1 upto start
			 do (read-byte input)))
	  for byte = (read-byte input)
	  do (setf integer (logior integer (ash (logand byte #x7F) shift)))
	  do (incf shift 7)
	  do (incf start 1)
	  until (zerop (logand byte #x80))
	  finally (setf integer (if (plusp (logand byte #x40))
				    (logior integer (ash -1 shift))
				    integer)))))
    (values integer start)))
