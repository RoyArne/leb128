
(in-package #:cl-user)

(defpackage #:leb128-asd
  (:use #:cl #:asdf))

(in-package #:leb128-asd)

(defsystem leb128
  :name "leb128"
  :version "0.0.1"
  :serial t
  :components ((:file "leb128")))
