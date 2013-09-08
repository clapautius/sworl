
;;;; A particle = an object that has position, velocity, acceleration and mass.
(defclass particle ()
  ((location
    :initarg :location
    :initform (make-array 3 :initial-element 0)
    :accessor location)

   (velocity
    :initarg :velocity
    :initform (make-array 3 :initial-element 0)
    :accessor velocity)

   (acceleration
    :initarg :acceleration
    :initform (make-array 3 :initial-element 0)
    :accessor acceleration)

   (mass
    :initarg :mass
    :initform 0
    :accessor mass))

  (:documentation "A particle = an object that has position, velocity, acceleration and
  mass."))

                  
(defgeneric update (entity)
  (:documentation "Update parameters for a generic entity."))


(defmethod update ((particle particle))
  "Update location, velocity and acceleration for a particle."

  ;; update velocity
  (dotimes (i (length (velocity particle)))
    (setf (aref (velocity particle) i)
          (+ (aref (velocity particle) i) (aref (acceleration particle) i))))
  
  ;; update location
  (dotimes (i (length (location particle)))
    (setf (aref (location particle) i)
          (+ (aref (location particle) i) (aref (velocity particle) i)))))


(defgeneric apply-force (force entity)
  (:documentation "Apply a force to a generic entity."))


(defmethod apply-force (force (particle particle))
  "Apply a force to a particle (by changing its acceleration according to F=m x a).
FORCE must be a vector with the same number of elements as the acceleration parameter of
the PARTICLE."
  (when (/= (length force) (length (acceleration particle)))
    (error "Force and acceleration don't have the same number of elements."))
  (dotimes (i (length force))
    (incf (aref (acceleration particle) i) (/ (aref force i) (mass particle)))))


(defmacro particle-vect-x (particle parameter)
  `(aref (,parameter ,particle) 0))

(defmacro particle-vect-y (particle parameter)
  `(aref (,parameter ,particle) 1))

(defmacro particle-vect-z (particle parameter)
  `(aref (,parameter ,particle) 2))


(defmacro particle-loc-x (particle)
  `(aref (location ,particle) 0))

(defmacro particle-loc-y (particle)
  `(aref (location ,particle) 1))

(defmacro particle-loc-z (particle)
  `(aref (location ,particle) 2))


(defun vector-magnitude (vect)
  "Compute a vector's magnitude."
  (let ((mag 0))
    (dotimes (i (length vect))
      (incf mag (* (aref vect i) (aref vect i))))
    (sqrt mag)))


(defun vector-multiply (vect scalar)
  "Multiply a vector with a scalar value.
Return a vector of the same dimension."
  (dotimes (i (length vect))
    (setf (aref vect i) (* (aref vect i) scalar))))


(defun vector-divide (vect scalar)
  "Divide a vector by a scalar value.
Return a vector of the same dimension."
  (dotimes (i (length vect))
    (setf (aref vect i) (/ (aref vect i) scalar))))


(defun vector-normalize (vect)
  "Normalize a vector."
  (vector-divide vect (vector-magnitude vect)))


(defmethod distance-3d-fast ((part1 particle) (part2 particle))
  "Compute distance between two particles (without the sq. root)."
  (let ((x-diff (- (particle-vect-x part1 location) (particle-vect-x part2 location)))
        (y-diff (- (particle-vect-y part1 location) (particle-vect-y part2 location)))
        (z-diff (- (particle-vect-z part1 location) (particle-vect-z part2 location))))
    (+ (* x-diff x-diff) (* y-diff y-diff) (* z-diff z-diff))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
