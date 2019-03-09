(defsystem binheap
  :name "binheap"
  :version "0.1"
  :author "karl"
  :license "UNLICENSE"
  :description "Binary Heap implementation"
  :components ((:file "package")
               (:file "binheap" :depends-on ("package"))))
