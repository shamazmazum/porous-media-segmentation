(in-package :pm-segmentation)

(sera:-> segments-location
         ((simple-array fixnum (* *)))
         (values (simple-array circle (*)) &optional))
(defun segments-location (segments)
  "Return center and radius of the smallest enclosing circle for each
segment in a labelled image SEGMENTS (a result of LABEL
function). This function returns two arrays. The first one contains
centers and the second contains radii."
  (declare (type (simple-array fixnum (* *)) segments))
  (let* ((nsegments (reduce #'max (aops:flatten segments)))
         (points (make-array nsegments :initial-element nil)))

    (array-operations/utilities:nested-loop (i j)
        (array-dimensions segments)
      (let ((segment (aref segments i j)))
        (when (not (zerop segment))
          (push (cons (float i) (float j))
                (aref points (1- segment))))))

    (aops:vectorize* 'circle (points)
      (smallest-enclosing-circle points))))
