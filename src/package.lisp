(defpackage porous-media-segmentation
  (:use #:cl)
  (:nicknames #:pm-segmentation)
  (:local-nicknames (:alex  :alexandria)
                    (:sera  :serapeum)
                    (:float :float-features))
  (:export #:label
           #:segments-location
           #:pair
           ;; For testing
           #:smallest-enclosing-circle
           #:naïve-smallest-enclosing-circle-radius))
