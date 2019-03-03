(defpackage :binhp
  (:documentation "Binary heap implementation")
  (:use :cl)
  (:export #:make-heap
           #:insert
           #:extract
           #:print-tree))