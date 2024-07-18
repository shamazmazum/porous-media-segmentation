(in-package :pm-segmentation)

(sera:-> make-disk
         (alex:positive-fixnum)
         (values (simple-array single-float (* *)) &optional))
(defun make-disk (n)
  (declare (type alex:positive-fixnum n))
  (let* ((side (1+ (* n 2)))
         (disk (make-array (list side side)
                           :element-type 'single-float)))
    (aops:each-index (i j)
      (setf (aref disk i j)
            (if (<= (+ (expt (- i n) 2)
                       (expt (- j n) 2))
                    (expt n 2))
                1.0 0.0)))
    disk))

(sera:-> make-gaussian-filter
         ((single-float 0.0))
         (values (simple-array single-float (* *)) &optional))
(defun make-gaussian-filter (sigma)
  (declare (type (single-float 0.0) sigma))
  (let* ((side (1+ (ceiling (* 4.0 sigma))))
         (center (ash side -1))
         (filter (make-array (list side side)
                             :element-type 'single-float)))
    (aops:each-index (i j)
      (setf (aref filter i j)
            (exp (- (/ (+ (expt (- i center) 2)
                          (expt (- j center) 2))
                       (* 2.0 (expt sigma 2)))))))
    ;; Normalize
    (let ((sum (reduce #'+ (aops:flatten filter))))
      (aops:vectorize! filter (filter) (/ filter sum)))))

(sera:-> prune-peaks
         ((simple-array bit          (* *))
          (simple-array single-float (* *)))
         (values (simple-array bit   (* *)) &optional))
(defun prune-peaks (peaks edt)
  (declare (type (simple-array bit          (* *)) peaks)
           (type (simple-array single-float (* *)) edt)
           (optimize (speed 3)))
  (let (peaks-list)
    (array-operations/utilities:nested-loop (i j)
        (array-dimensions peaks)
      (when (not (zerop (aref peaks i j)))
        (push (cons i j) peaks-list)))
    (flet ((floats (pair)
             (declare (type (cons fixnum fixnum) pair))
             (cons (float (car pair))
                   (float (cdr pair)))))
      (let* ((vp-tree (vp-trees:make-vp-tree
                       peaks-list #'squared-distance
                       :key #'floats))
             (pruned-list
              (reduce
               (lambda (acc peak)
                 (let* ((neighbors (vp-trees:items-in-ball
                                    vp-tree peak
                                    (aref edt (car peak) (cdr peak))
                                    #'squared-distance
                                    :key #'floats))
                        (farthest (reduce (lambda (peak1 peak2)
                                            (if (> (aref edt (car peak1) (cdr peak1))
                                                   (aref edt (car peak2) (cdr peak2)))
                                                peak1 peak2))
                                          neighbors)))
                   (union acc (remove farthest neighbors :test #'equal) :test #'equal)))
               peaks-list :initial-value nil))
             (peaks-list
              (set-difference peaks-list pruned-list :test #'equal))
             (result (make-array (array-dimensions peaks)
                                 :element-type 'bit
                                 :initial-element 0)))
        (dolist (peak peaks-list)
          (setf (aref result (car peak) (cdr peak)) 1))
        result))))

(sera:-> label
         (imago:binary-image
          &key
          (:sigma       (single-float 0.0))
          (:radius      alex:positive-fixnum)
          (:join-near-p boolean))
         (values (simple-array fixnum (* *)) &optional))
(defun label (image &key (sigma 0.4) (radius 4) join-near-p)
  "Perform segmentation of a black&white image (of type
IMAGO:BINARY-IMGAGE). Black areas will be assigned the label zero and
white areas will be assigned some positive label.

Segmentation is performed using a simplyfied snow algorithm with
radius of Gaussian filter SIGMA and size of a circular structuring
element (1+ (* 2 RADIUS)).

When JOIN-NEAR-P is T an LABEL will try to join near segments to
avoid oversegmentation (experimental feature)."
  (declare (type imago:binary-image image)
           (type (single-float 0.0) sigma)
           (type alex:positive-fixnum radius)
           (type boolean join-near-p)
           (optimize (speed 3)))
  (let* ((pixels (imago:image-pixels image))
         (inverted (aops:vectorize* 'bit (pixels) (- 1 pixels))) ; Invert image
         (edt (imago:distance-transform
               (make-instance 'imago:binary-image :pixels inverted)
               :type :edt))
         (edt (cl-watershed:convolve
               (aops:vectorize* 'single-float
                   (edt)
                 (float edt))
               (make-gaussian-filter sigma))) ; Calculate EDT and apply blur
         (dilation (cl-watershed:convolve edt (make-disk radius) #'max))
         (peaks-image
          (make-instance 'imago:binary-image
                         :pixels (let ((peaks
                                        ;; Calculate initial seeds
                                        (aops:vectorize* 'bit (dilation edt inverted)
                                                (if (and (= dilation edt)
                                                         (zerop inverted))
                                                    1 0))))
                                   (if join-near-p (prune-peaks peaks edt) peaks)))))
    (declare (type (simple-array bit (* *)) pixels inverted))
    (cl-watershed:watershed
     edt (imago:label-components peaks-image)
     (aops:vectorize* 'boolean (inverted) (zerop inverted)))))
