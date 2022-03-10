(in-package :pm-segmentation-tests)

(defparameter *disks-image*
  (asdf:system-relative-pathname
   :porous-media-segmentation/tests "tests/disks.pbm"))

(defun run-tests ()
  (let ((status (run 'pm-segmentation)))
    (explain! status)
    (results-status status)))

(defun ≈ (x1 x2 &optional (tol 5f-4))
    (< (abs (- x1 x2)) tol))

(def-suite pm-segmentation :description "Test segmentation")
(in-suite pm-segmentation)

(test segmentation
  (let ((segments (pm-segmentation:label (imago:read-image *disks-image*))))
    (is (= (reduce #'max (aops:flatten segments)) 4))
    (let* ((amount (loop for segment from 1 to 4 collect
                        (count segment (aops:flatten segments))))
           (total (reduce #'+ amount)))
      (is-true
       (every
        (lambda (amount)
          (< 0.2 (/ amount total) 0.3))
        amount)))))

(test smallest-enclosing-circle
  (loop
     repeat 10
     for points = (loop repeat 100 collect
                       (cons (- (random 200.0) 100.0)
                             (- (random 200.0) 100.0)))
     do
       (multiple-value-bind (center1 radius1)
           (serapeum:deconstruct (pm-segmentation:smallest-enclosing-circle points))
         (multiple-value-bind (center2 radius2)
             (serapeum:deconstruct (pm-segmentation:naïve-smallest-enclosing-circle points))
           (is (≈ radius1 radius2))
           (is (≈ (car center1) (car center2)))
           (is (≈ (cdr center1) (cdr center2)))))))

(test size-extraction
  (let* ((segment-sizes (pm-segmentation:segments-location
                         (pm-segmentation:label
                          (imago:read-image *disks-image*))))
         (first-segment-size (aref segment-sizes 0)))
    (is-true
     (every
      (lambda (rel-size) (< 0.9 rel-size 1.1))
      (aops:vectorize* 'single-float (segment-sizes)
        (/ (pm-segmentation:circle-radius segment-sizes)
           (pm-segmentation:circle-radius first-segment-size)))))))
