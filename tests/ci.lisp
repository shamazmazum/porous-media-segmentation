(defun do-all()
  (ql:quickload :porous-media-segmentation/tests :verbose t)
  (uiop:quit
   (if (uiop:call-function "pm-segmentation-tests:run-tests")
       0 1)))

(do-all)
