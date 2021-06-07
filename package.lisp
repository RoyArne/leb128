;;;; leb128/package.lisp
;;;;
(in-package :cl-user)

(defpackage :roy-arne.gangstad.leb128
  (:use :cl)
  (:export :unsigned-length
	   :encode-unsigned
	   :decode-unsigned
	   
	   :signed-length
	   :encode-signed
	   :decode-signed))

