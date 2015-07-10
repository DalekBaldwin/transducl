;;;; transducl.asd

(defpackage :transducl-system
  (:use :cl :asdf))
(in-package :transducl-system)

(defsystem :transducl
  :name "transducl"
  :serial t
  :components
  ((:static-file "transducl.asd")
   (:module :src
            :components ((:file "package")
                         (:file "transducl"))
            :serial t))
  :depends-on (:alexandria)
  :in-order-to ((test-op (load-op :transducl-test)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern #.(string '#:run-all-tests)
                             :transducl-test))))

(defsystem :transducl-test
  :name "transducl-test"
  :serial t
  :description "Tests for transducl."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "transducl-test"))))
  :depends-on (:transducl :stefil))
