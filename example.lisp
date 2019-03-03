;;;; Examples illustrating the use of the binary heap implementation.

;;; Loading the library, adjust the paths to your own directories.
;;; Can also be done by loading the source files directly.
(if (not (member #p"~/portacle/projects/"
                 asdf:*central-registry*))
    (push #p"~/portacle/projects/"
          asdf:*central-registry*))
(ql:quickload :binheap)

;;; Max-heaps
;; Let's build a heap of integers ordered from biggest to smallest.
(defparameter *arr* (make-array 6 :fill-pointer 6
                          :initial-contents (list 3 4 1 2 5 6)))
(defparameter *heap* (binhp:make-heap *arr* #'>=))
;; #'>= is the relation enforced throughout the heap between every parent node
;; and its children.
(binhp:print-tree *heap*)
;; =>
;; 6 
;; 5 3 
;; 2 4 1

;; Alright, this is a nice heap.
;; You can insert elements in it:
(binhp:insert *heap* 3.5)
(binhp:print-tree *heap*)
;; =>
;; 6 
;; 5 3.5 
;; 2 4 1 3

;; The new element fits in the heap.
;; You can pop elements to get the successive biggest of the heap:
(loop for it from 0 below (length *arr*) do
      (format t "~a " (binhp:extract *heap*)))
(terpri t)
;; => 6 5 4 3.5 3 2 1

;;; The same goes for Min-heaps, just replace #'>= with #'<=.
;;; You can define any relation that is a total order in 'test.

;;; Alphabetical heap.
;; The heap implementation works for any element types and any total order.
;; Let's put some strings in an alphabetical order heap.
(defparameter *arr*
  (make-array 5
              :fill-pointer 5
              :initial-contents (list "Pierre" "Jacques" "Paul" "Jean" "Luc")))
(defparameter *heap* (binhp:make-heap *arr* #'string-lessp))
(binhp:print-tree *heap*)
;; =>
;; Jacques 
;; Jean Paul 
;; Pierre Luc
(loop for it from 0 below (length *arr*) do
      (format t "~a " (binhp:extract *heap*)))
(terpri t)
;; => Jacques Jean Luc Paul Pierre
