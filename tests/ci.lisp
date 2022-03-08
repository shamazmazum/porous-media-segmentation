(defun do-all()
  (ql:quickload :sandcorn-distribution/tests :verbose t)
  (uiop:quit
   (if (uiop:call-function "sandcorn-distribution-tests:run-tests")
       0 1)))

(do-all)
