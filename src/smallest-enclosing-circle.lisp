(in-package :pm-segmentation)
(deftype pair (type) `(cons ,type ,type))

(sera:-> triangle=>circumscribed-circle
         ((pair single-float)
          (pair single-float)
          (pair single-float))
         (values (pair single-float) single-float &optional))
(defun triangle=>circumscribed-circle (a b c)
  (declare (type (pair single-float) a b c)
           (optimize (speed 3)))
  ;; Move a to the origin.
  (let* ((ax (car a))
         (ay (cdr a))

         (bx (- (car b) ax))
         (by (- (cdr b) ay))

         (cx (- (car c) ax))
         (cy (- (cdr c) ay))

         (d (* 2 (- (* bx cy) (* by cx))))
         (ux (/ (- (* cy (+ (expt bx 2) (expt by 2)))
                   (* by (+ (expt cx 2) (expt cy 2))))
                d))
         (uy (/ (- (* bx (+ (expt cx 2) (expt cy 2)))
                   (* cx (+ (expt bx 2) (expt by 2))))
                d)))

    (values
     (cons (+ ax ux)
           (+ ay uy))
     (sqrt (+ (expt ux 2)
              (expt uy 2))))))

(sera:-> segment=>circumscribed-circle
         ((pair single-float)
          (pair single-float))
         (values (pair single-float) single-float &optional))
(defun segment=>circumscribed-circle (a b)
  (declare (type (pair single-float) a b)
           (optimize (speed 3)))
  (let ((ax (car a))
        (ay (cdr a))
        (bx (car b))
        (by (cdr b)))

    (values
     (cons (/ (+ ax bx) 2)
           (/ (+ ay by) 2))
     (sqrt (+ (expt (/ (- ax bx) 2) 2)
              (expt (/ (- ay by) 2) 2))))))

(sera:-> point-in-circle-p
         ((pair single-float)
          (pair single-float)
          single-float)
         (values boolean &optional))
(defun point-in-circle-p (point center radius)
  (declare (optimize (speed 3))
           (type (pair single-float) point center)
           (type single-float radius))
  (let ((px (car point))
        (py (cdr point))
        (cx (car center))
        (cy (cdr center)))

    (< (+ (expt (- px cx) 2)
          (expt (- py cy) 2))
       (+ (expt radius 2) 1f-8))))

(sera:-> naïve-smallest-enclosing-circle-radius
         (list) (values single-float &optional))
(defun naïve-smallest-enclosing-circle-radius (points)
  (declare (optimize (speed 3)))
  (let ((combinations-2 (snakes:combinations points 2))
        (combinations-3 (snakes:combinations points 3)))
    (declare (type function combinations-2 combinations-3))
    ;; Two points on the boundary of the smallest circle
    (min
     (loop
        for (p1 p2) = (multiple-value-list (funcall combinations-2))
        until (eq p1 'snakes:generator-stop) minimize
          (multiple-value-bind (center radius)
              (segment=>circumscribed-circle p1 p2)
            (if (every (lambda (point)
                         (point-in-circle-p point center radius))
                       (set-difference points (list p1 p2) :test #'equal))
                radius float:single-float-positive-infinity))
        single-float)
     ;; Three or more points on the boundary of the smallest circle
     (loop
        for (p1 p2 p3) = (multiple-value-list (funcall combinations-3))
        until (eq p1 'snakes:generator-stop) minimize
          (multiple-value-bind (center radius)
              (triangle=>circumscribed-circle p1 p2 p3)
            (if (every (lambda (point)
                         (point-in-circle-p point center radius))
                       (set-difference points (list p1 p2 p3) :test #'equal))
                radius float:single-float-positive-infinity))
          single-float))))

(sera:-> random-permutation (list) (values list &optional))
(defun random-permutation (list)
  (labels ((permute% (list acc)
             (if (null list) acc
                 (let ((idx (random (length list))))
                   (permute%
                    (append
                     (subseq list 0 idx)
                     (subseq list (1+ idx)))
                    (cons
                     (nth idx list)
                     acc))))))
    (permute% list nil)))

;; Implementation of Welzl's algorithm (NB: Uses stack!)
(sera:-> smallest-enclosing-circle
         (list)
         (values (pair single-float) single-float &optional))
(defun smallest-enclosing-circle (points)
  "Return a center and a radius of the smalled enclosing circle for a
list of points. Each point is a cons of two single-float numbers."
  (labels ((trivial (boundary)
             (let ((length (length boundary)))
               (ecase length
                 (0 (values (cons 0.0 0.0) 0.0))
                 (1 (values (car boundary) 0.0))
                 (2 (apply #'segment=>circumscribed-circle boundary))
                 (3 (apply #'triangle=>circumscribed-circle boundary)))))
           (scr% (points boundary)
             (if (or (null points)
                     (= (length boundary) 3))
                 (trivial boundary)
                 (let ((first (first points)))
                   (multiple-value-bind (center radius)
                       (scr% (cdr points) boundary)
                     (if (point-in-circle-p first center radius)
                         (values center radius)
                         (scr% (cdr points) (cons first boundary))))))))
    (scr% (random-permutation points) nil)))
