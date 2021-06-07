;;;; leb128/leb128.asd
;;;;
(in-package :cl-user)

(defpackage :roy-arne.gangstad.leb128-asd
  (:use :cl :asdf))

(in-package :roy-arne.gangstad.leb128-asd)

(defsystem leb128
  :name "leb128"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "leb128")))
