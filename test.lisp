;;;; Simple tests for validating binheap.

;;; Loading the library, adjust the paths to your own directories.
(asdf:load-system :binheap)

;;; Validation
(format t "~%  -- Testing binheap --~%")
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
	      (setf arrval (sort arrval test))
              (setf hp (binhp:make-heap arr test))
	      ;; Test the peek
	      (assert (= (binhp:peek hp) (aref arrval 0)))
              ;; Now pop all the elements and verify that we get the right order
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
(format t "-------------------------~%")
