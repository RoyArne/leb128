;;;; Copyright 2021 Roy Arne Gangstad
;;;; 
;;;; Licensed under the Apache License, Version 2.0 (the "License");
;;;; you may not use this file except in compliance with the License.
;;;; You may obtain a copy of the License at
;;;; 
;;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;; 
;;;; Unless required by applicable law or agreed to in writing, software
;;;; distributed under the License is distributed on an "AS IS" BASIS,
;;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;;; See the License for the specific language governing permissions and
;;;; limitations under the License.

(in-package #:cl-user)

(defpackage #:leb128-asd
  (:use #:cl #:asdf))

(in-package #:leb128-asd)

(defsystem leb128
  :name "leb128"
  :author "Roy Arne Gangstad <roy.gangstad@gmail.com>"
  :maintainer "Roy Arne Gangstad <roy.gangstad@gmail.com>"
  :description "Encode and decode integers in the LEB128 format."
  :license "Apache 2.0"
  :version "1"
  :components ((:file "leb128")))
