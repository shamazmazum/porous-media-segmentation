(in-package :pm-segmentation-tests)

(defparameter *disks-image*
  (asdf:system-relative-pathname
   :porous-media-segmentation/tests "tests/disks.pbm"))

(defun run-tests ()
  (let ((status (run 'pm-segmentation)))
    (explain! status)
    (results-status status)))

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
       (is-true
        (< (abs (- (nth-value 1 (pm-segmentation:smallest-enclosing-circle points))
                   (pm-segmentation:naïve-smallest-enclosing-circle-radius points)))
           5f-4))))

(test size-extraction
  (let* ((segment-sizes (nth-value 
                         1 (pm-segmentation:segments-location
                            (pm-segmentation:label
                             (imago:read-image *disks-image*)))))
         (first-segment-size (aref segment-sizes 0)))
    (is-true
     (every
      (lambda (rel-size) (< 0.9 rel-size 1.1))
      (aops:vectorize* 'single-float (segment-sizes)
        (/ segment-sizes first-segment-size))))))
