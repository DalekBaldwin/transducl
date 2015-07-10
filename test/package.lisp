(in-package :cl-user)

(defpackage :transducl-test
  (:use :cl :transducl :stefil :alexandria)
  (:export
   #:test-all))

(in-package :transducl-test)

(defparameter *system-directory* transducl::*system-directory*)
