(in-package :pm-segmentation)
(deftype pair (type) `(cons ,type ,type))

(sera:defconstructor circle
  (center (pair single-float))
  (radius (single-float 0f0)))

(sera:-> triangle=>circumscribed-circle
         ((pair single-float)
          (pair single-float)
          (pair single-float))
         (values circle &optional))
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

    (circle
     (cons (+ ax ux)
           (+ ay uy))
     (sqrt (+ (expt ux 2)
              (expt uy 2))))))

(sera:-> segment=>circumscribed-circle
         ((pair single-float)
          (pair single-float))
         (values circle &optional))
(defun segment=>circumscribed-circle (a b)
  (declare (type (pair single-float) a b)
           (optimize (speed 3)))
  (let ((ax (car a))
        (ay (cdr a))
        (bx (car b))
        (by (cdr b)))

    (circle
     (cons (/ (+ ax bx) 2)
           (/ (+ ay by) 2))
     (sqrt (+ (expt (/ (- ax bx) 2) 2)
              (expt (/ (- ay by) 2) 2))))))

(sera:-> squared-distance
         ((pair single-float)
          (pair single-float))
         (values (single-float 0f0) &optional))
(defun squared-distance (p1 p2)
  (declare (optimize (speed 3))
           (type (pair single-float) p1 p2))
  (let ((x1 (car p1))
        (y1 (cdr p1))
        (x2 (car p2))
        (y2 (cdr p2)))
    (+ (expt (- x1 x2) 2)
       (expt (- y1 y2) 2))))

(sera:-> point-in-circle-p
         ((pair single-float)
          circle)
         (values boolean &optional))
(defun point-in-circle-p (point circle)
  (declare (optimize (speed 3))
           (type (pair single-float) point)
           (type circle circle))
  (multiple-value-bind (center radius)
      (sera:deconstruct circle)
    (declare (type single-float radius))
    (< (squared-distance center point)
       (+ (expt radius 2) 1f-6))))

(defmacro with-multi-mode ((mode) &body body)
  `(let ((snakes:*snakes-multi-mode* ,mode))
     ,@body))

(defun foldl (function init-value generator)
  (declare (optimize (speed 3))
           (type function function generator))
  (labels ((foldl% (acc)
             (let ((value (funcall generator)))
               (if (eq value 'snakes:generator-stop)
                   acc
                   (foldl% (funcall function acc value))))))
    (foldl% init-value)))

(sera:-> naïve-smallest-enclosing-circle
         (list) (values circle &optional))
(defun naïve-smallest-enclosing-circle (points)
  (declare (optimize (speed 3)))
  (let ((biggest-circle (circle (cons 0.0 0.0)
                                float:single-float-positive-infinity)))
    (labels ((accumulate-circle (smallest-circle boundary-points)
               (declare (type list boundary-points))
               (let ((circle (ecase (length boundary-points)
                               (2 (apply #'segment=>circumscribed-circle boundary-points))
                               (3 (apply #'triangle=>circumscribed-circle boundary-points)))))
                 (if (and (every (lambda (point)
                                   (point-in-circle-p point circle))
                                 (set-difference points boundary-points :test #'equal))
                          (< (circle-radius circle)
                             (circle-radius smallest-circle)))
                     circle smallest-circle)))
             (smallest-circle (n-boundary-points)
               (foldl #'accumulate-circle biggest-circle
                    (with-multi-mode (:list)
                      (snakes:combinations points n-boundary-points)))))
      (let ((2-boundary-points (smallest-circle 2))
            (3-boundary-points (smallest-circle 3)))
        (if (< (circle-radius 2-boundary-points)
               (circle-radius 3-boundary-points))
            2-boundary-points 3-boundary-points)))))

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
         (values circle &optional))
(defun smallest-enclosing-circle (points)
  "Return a center and a radius of the smalled enclosing circle for a
list of points. Each point is a cons of two single-float numbers."
  (labels ((trivial (boundary)
             (let ((length (length boundary)))
               (ecase length
                 (0 (circle (cons 0.0 0.0) 0.0))
                 (1 (circle (car boundary) 0.0))
                 (2 (apply #'segment=>circumscribed-circle boundary))
                 (3 (apply #'triangle=>circumscribed-circle boundary)))))
           (scr% (points boundary)
             (if (or (null points)
                     (= (length boundary) 3))
                 (trivial boundary)
                 (let ((first (first points))
                       (circle (scr% (cdr points) boundary)))
                   (if (point-in-circle-p first circle)
                       circle
                       (scr% (cdr points) (cons first boundary)))))))
    (scr% (random-permutation points) nil)))
