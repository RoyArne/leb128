;;;; leb128/leb128.lisp
;;;;
(in-package :roy-arne.gangstad.leb128)


(defun unsigned-length (integer)
  "Return the length of INTEGER in the unsigned LEB-128 format."
  
  (declare (type integer integer))

  (max 1 (ceiling (integer-length integer) 7)))



(defun encode-unsigned (integer &optional vector (start 0))
  "Return a (vector (unsigned-byte 8)) encoding INTEGER,
and the end index of the encoded value.

INTEGER is encoded in the unsigned LEB-128 format.

If VECTOR is given then it must be a (vector (unsigned-byte 8)),
and INTEGER is encoded into it beginning at START."
  
  (declare (type integer integer)
	   (type (or null (vector (unsigned-byte 8))) vector)
	   (type fixnum start))

    (loop
       with length =  (unsigned-length integer)
       with end = (+ start length)
       with out = (or vector (make-array length :element-type '(unsigned-byte 8)))

       for i from start below end
	 
       for byte = (logand integer #x7F)
       do (setf integer (ash integer -7))
       do (setf (aref out i) (if (zerop integer)
				 byte
				 (logior byte #x80)))
       finally (return (values out i))))



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



(defun encode-signed (integer &optional vector (start 0))
  "Return a (vector (unsigned-byte 8)) encoding INTEGER,
and the end index of the encoded value.

INTEGER is encoded in the signed LEB-128 format.

If VECTOR is given then it must be a (vector (unsigned-byte 8)),
and INTEGER is encoded into it beginning at START."
  
  (declare (type integer integer)
	   (type (or null (vector (unsigned-byte 8))) vector)
	   (type fixnum start))

  (loop
     with length = (signed-length integer)
     with end = (+ start length)
     with out = (or vector (make-array length :element-type '(unsigned-byte 8)))

     for i from start below end
       
     for byte = (logand integer #x7F)
     do (setf integer (ash integer -7))
     do (setf (aref out i) (if (= i (1- end))
			       byte
			       (logior byte #x80)))

     finally (return (values out end))))



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
