;;;; Binary heap implementation.
;;;; We implemented this structure using the excellent article at:
;;;; https://en.wikipedia.org/wiki/Binary_heap
;;;; The binary heap is implemented as a CLOS class.
;;;; Please note that the vector you provide to the heap object is used in-place
;;;; throughout the life of the heap. It is up to you to make copies and ensure
;;;; the vector is not modified externally.

(in-package :binhp)

(defclass heap ()
  ((vec
    :documentation "Resizeable array to store the implicit Binary Heap."
    :initarg :vec
    :initform (error "class heap: Please provide a vector")
    :accessor vec)
   (test
    :documentation "Total order function. The heap enforces:
{funcall totorder parent child} throughout the binary tree."
    :initarg :test
    :initform (error "class heap: Please provide a total order relation.")
    :accessor test)))

(defun make-heap (vec test)
  "Heap constructor.
I: vec: Vector to back the implicit binary tree structure. Works in place.
        Must have a fill-pointer.
   test: The total order to enforce throughout the heap. 
         [funcall test parent child] is true throughout the tree."
  (assert (array-has-fill-pointer-p vec))
  (assert (typep test 'function))
  (let ((hp (make-instance 'heap :vec vec :test test))) 
    (build hp)
    hp))

;;; Main
(defmethod build ((tree heap))
  "Initial building of the binary heap from the input vector of data.
Sub-method."
  ;; We work our way up the tree calling the down-heap method on each parent
  ;; node.
  ;; Parent nodes are the ones from 0 to floor{n-2 / 2} included.
  (loop for ind from (floor (/ (- (length (vec tree)) 2) 2)) downto 0 do
    (down-heap tree ind)))

(defmethod insert ((tree heap) newkey)
  "Push a new element to the heap.
I: * Heap instance.
   * New element."
  (with-slots ((arr vec)
               (test test)) tree
    ;; Inserts a new element at the end of arr and performs a up-heap.
    ;; Last element of the array is guaranteed to be a leaf of the tree.
    (vector-push-extend newkey arr)
    ;; Compare the new element with its parent node.
    ;;   * If order is respected or if we've reached the root of the tree
    ;;     then return.
    ;;   * Else swap. And repeat.
    (let* ((ind (1- (length arr)))
           (parind (floor (/ (1- ind) 2))))
      (loop while (and (not (= ind 0))
                       (not (funcall test (aref arr parind)
                                     (aref arr ind)))) do
	       (rotatef (aref arr parind) (aref arr ind))
	       (setf ind parind)
	       (setf parind (floor (/ (1- ind) 2)))))))

(defmethod down-heap ((tree heap) ind)
  "Perform the down-heap operation. Move the parent node at 'ind' downwards
until it settles in a suitable position. Sub-method, not exposed to user."
  ;; Compare the current key with its two children. Return if order is respected
  ;; else swap the current key with the child that respects the total order with
  ;; the other child. Also return if we have reached a leaf of the tree.
  ;; Nodes at an index starting at ceil{n-2 / 2} are leafs.
  (with-slots ((arr vec)
               (test test)) tree
    (let* ((maxind (1- (length arr)))
           (leaf-limit (floor (/ (1- maxind) 2)))
           (left-child (+ (* 2 ind) 1))
           (right-child (min (1+ left-child) maxind)))
      (loop while
            (and
             ;; Order of tests matters here!
             (not (> ind leaf-limit))
             (not (and (funcall test (aref arr ind) (aref arr left-child))
                       (funcall test (aref arr ind) (aref arr right-child)))))
            do
            ;; Find out the right child to swap with and swap.
            (if (funcall test (aref arr left-child) (aref arr right-child))
                (progn (rotatef (aref arr ind) (aref arr left-child))
                       (setf ind left-child))
                (progn (rotatef (aref arr ind) (aref arr right-child))
                       (setf ind right-child)))
            (setf left-child (+ (* 2 ind) 1))
            (setf right-child (min (1+ left-child) maxind))))))

(defmethod extract ((tree heap))
  "Pop the root element from the heap. Rearranges the tree afterwards.
I: Heap instance.
O: Root element."
  (with-slots ((arr vec)) tree
    (let ((root (aref arr 0)))
      ;; replace the root with the last leaf
      ;; resize vector
      ;; down-heap the new root.
      (setf (aref arr 0) (aref arr (1- (length arr))))
      (vector-pop arr)
      (down-heap tree 0)
      root)))

(defmethod print-tree ((tree heap))
  "Print the whole tree in a very basic formatting, level by level 
and left to right."
  (with-slots ((arr vec)) tree
    (let* ((n (length arr))
           (h (floor (log n 2))))
      ;; The heap is already ordered by level. And each level is in the right
      ;; order.
      (loop for level from 0 upto h do
	(loop for ind from (1- (expt 2 level))
		below (1- (expt 2 (1+ level))) do
                  (when (< ind n)
                      (format t "~a " (aref arr ind))))
	(terpri t)))))

(defmethod size ((tree heap))
  (length (vec tree)))

(defmethod peek ((tree heap))
  (with-slots ((arr vec)) tree
    (aref arr 0)))

(defmethod peek-second ((tree heap))
  (with-slots ((arr vec)
	       (pred test)) tree
    (when (= 2 (size tree)) (return-from peek-second (aref arr 1)))
    (if (funcall pred ( aref arr 1) (aref arr 2))
	(aref arr 1)
	(aref arr 2))))
