;;;; various functions related to newtonian simulations (gravity & co.)

(defun orbital-speed (mass-big mass-small distance)
  (sqrt (/ (* mass-big mass-big 6.67384e-11) (* (+ mass-big mass-small) distance))))


(defun newton-law-gravity-3d (obj1 obj2)
  "Compute forces between objects OBJ1 and OBJ2 according to Newton's law of gravity.
OBJ1 and OBJ2 must be PARTICLE instances."
  (let* ((dist (distance-3d-fast obj1 obj2))
         ;; components of the force vector (from obj1 to obj2)
         (force-x (- (particle-loc-x obj2) (particle-loc-x obj1)))
         (force-y (- (particle-loc-y obj2) (particle-loc-y obj1)))
         (force-z (- (particle-loc-z obj2) (particle-loc-z obj1)))
         ;; force1 - from obj1 to obj2
         (force1 (make-array 3))
         ;; force2 - from obj2 to obj1
         (force2 (make-array 3))
         (force-magnitude (/ (* (mass obj1) (mass obj2) 6.67384e-11) dist)))
    (setf (aref force1 0) force-x)
    (setf (aref force1 1) force-y)
    (setf (aref force1 2) force-z)
    (setf (aref force2 0) (- force-x))
    (setf (aref force2 1) (- force-y))
    (setf (aref force2 2) (- force-z))
    (vector-normalize force1)
    (vector-normalize force2)
    (vector-multiply force1 force-magnitude)
    (vector-multiply force2 force-magnitude)
    (apply-force force1 obj1)
    (apply-force force2 obj2)))


(defun make-newton-universe ()
  "Create a universe containing Newton's universal law of gravity."
  (let ((newton-universe (make-instance 'universe)))
    (setf (rules-pair newton-universe)
          (cons 'newton-law-gravity-3d (rules-pair newton-universe)))
    newton-universe))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
