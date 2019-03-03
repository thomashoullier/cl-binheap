(defsystem binheap
  :name "my-system"
  :version "0.1"
  :author "karl"
  :license "MIT"
  :description "Binary Heap implementation"
  ;;:serial t
  :components ((:file "package")
               (:file "binheap" :depends-on ("package"))))
