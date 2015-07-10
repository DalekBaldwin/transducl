(in-package :cl-user)

(defpackage :transducl
  (:use :cl :alexandria)
  (:export #:list-conj
           #:map-reducer
           #:filter-reducer
           #:mapping
           #:filtering
           #:filter-mapping
           #:mapcatting
           #:transduce
           #:super-transducer
           #:super-fold))

(in-package :transducl)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "transducl"))))
