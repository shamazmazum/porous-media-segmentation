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

(sera:-> label
         (imago:binary-image
          &key
          (:sigma  (single-float 0.0))
          (:radius alex:positive-fixnum))
         (values (simple-array fixnum (* *)) &optional))
(defun label (image &key (sigma 0.4) (radius 4))
  "Perform segmentation of a black&white image (of type
IMAGO:BINARY-IMGAGE). Black areas will be assigned the label zero and
white areas will be assigned some positive label.

Segmentation is performed using a simplyfied snow algorithm with
radius of Gaussian filter SIGMA and size of a circular structuring
element (1+ (* 2 RADIUS))."
  (declare (type imago:binary-image image)
           (type (single-float 0.0) sigma)
           (type alex:positive-fixnum radius)
           (optimize (speed 3)))
  (let* ((pixels (imago:image-pixels image))
         (inverted (aops:vectorize* 'bit (pixels) (- 1 pixels))) ; Invert image
         (edt (cl-watershed:convolve
               (imago:distance-transform
                (make-instance 'imago:binary-image :pixels inverted)
                :type :edt)
               (make-gaussian-filter sigma))) ; Calculate EDT and apply blur
         (dilation (cl-watershed:convolve edt (make-disk radius) #'max))
         (peaks-image
          (make-instance 'imago:binary-image
                         :pixels (aops:vectorize* 'bit (dilation edt inverted)
                                   (if (and (= dilation edt)
                                            (zerop inverted)) 1 0))))) ; Calculate initial seeds
    (declare (type (simple-array bit (* *)) pixels inverted))
    (cl-watershed:watershed
     edt (imago:label-components peaks-image)
     (aops:vectorize* 'boolean (inverted) (zerop inverted)))))
