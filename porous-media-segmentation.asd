(defsystem :porous-media-segmentation
  :name :porous-media-segmentation
  :description "Segmentation of two-phase (black & white) images of porous material"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :version "0.1"
  :depends-on (:array-operations
               :cl-watershed
               :imago
               :snakes
               :float-features
               :alexandria
               :serapeum)
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "segmentation")
               (:file "smallest-enclosing-circle")
               (:file "segments-location"))
  :in-order-to ((test-op (load-op "porous-media-segmentation/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (uiop:symbol-call :porous-media-segmentation-tests '#:run-tests)))

(defsystem :porous-media-segmentation/tests
  :name :porous-media-segmentation/tests
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :license "2-clause BSD"
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:porous-media-segmentation
               :fiveam
               :array-operations
               :imago))
