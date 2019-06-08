;;;; Simple tests for validating binheap.

;;; Loading the library, adjust the paths to your own directories.
(asdf:load-system :binheap)

;;; Validation
(format t "  -- Testing binheap --~%")
(format t "Creating empty or small binary heaps:... ")
(dotimes (it 10)
  (let ((arr (make-array it :fill-pointer it)))
    (binhp:make-heap arr #'>=)))
(format t "OK~%")

(format t "Simple heaps and operations:... ")
(loop for test in (list #'>= #'<=) do
      (loop for nelem from 10 upto 50 do
            (let ((arr (make-array nelem :fill-pointer nelem))
                  (arrval (make-array nelem))
                  (hp nil))
              (loop for ind from 0 below nelem do
                    (setf (aref arr ind) (random 100))
                    (setf (aref arrval ind) (aref arr ind)))
              (setf hp (binhp:make-heap arr test))
              ;; Now pop all the elements and verify that we get the right order
              (sort arrval test)
              (loop for ind from 0 below nelem do
                    (assert (= (binhp:extract hp) (aref arrval ind))))
              ;; Reinsert shuffled elements.
              (loop for ind from 0 below nelem do
                    (rotatef (aref arrval ind) (aref arrval (random nelem))))
              (loop for elem across arrval do
                    (binhp:insert hp elem))
              ;; Now repop everything and check order.
              (sort arrval test)
              (loop for ind from 0 below nelem do
                    (assert (= (binhp:extract hp) (aref arrval ind)))))))
(format t "OK~%")

;;; Performance
(terpri t) (format t "  -- Performance binheap -- ~%")
(loop for nelem in (list 100 10000 1000000) do
      (let ((arr (make-array nelem :element-type 'double-float
                                   :fill-pointer nelem
                                   :initial-element 0d0))
            (hp nil))
        (loop for ind from 0 below nelem do
              (setf (aref arr ind) (random 100d0)))
        (format t "Building a max-heap of ~a double-floats:~%" nelem)
        (time (setf hp (binhp:make-heap arr #'>=)))
        (format t "Popping a max-heap of ~a double-floats:~%" nelem)
        (time (dotimes (it nelem) (binhp:extract hp)))
        (format t "Reinserting ~a double-floats:~%" nelem)
        (time (dotimes (it nelem) (binhp:insert hp (random 100d0))))))
