;;;; Performance assessment for binheap.
(asdf:load-system :binheap)

(format t "~%  -- Performance binheap --~%")
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
(format t "-----------------------~%")
