(in-package :sworl)

;; :debug:
(declaim (optimize debug))

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
    :initform 1
    :accessor mass)

   ;; plist containing visual characteristics
   ;; :shape (= :cube | :sphere), :size (= number),
   ;; :color (= list of 3 numbers), :trail-color (= list of 3 numbers)
   (appearance
    :initarg :appearance
    :initform ()
    :accessor appearance))

  (:documentation "A particle = an object that has position, velocity, acceleration and
  mass."))


(defmethod print-object ((particle particle) stream)
  (format stream "particle: mass=~a, location=~a~%  velocity=~a, acceleration=~a~%"
          (mass particle) (location particle)
          (velocity particle) (acceleration particle)))


(defgeneric update (entity)
  (:documentation "Update parameters for a generic entity."))


(defmethod update ((particle particle))
  "Update location, velocity and acceleration for a particle."

  ;; update velocity and reset acceleration
  (dotimes (i (length (velocity particle)))
    (setf (aref (velocity particle) i)
          (+ (aref (velocity particle) i) (aref (acceleration particle) i)))
    (setf (aref (acceleration particle) i) 0))
  
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


;(defmacro particle-loc-x (particle)
;  `(aref (location ,particle) 0))
;
;(defmacro particle-loc-y (particle)
;  `(aref (location ,particle) 1))
;
;(defmacro particle-loc-z (particle)
;  `(aref (location ,particle) 2))

(defun particle-loc-x (particle)
  (aref (location particle) 0))

(defun particle-loc-y (particle)
  (aref (location particle) 1))

(defun particle-loc-z (particle)
  (aref (location particle) 2))


(defmethod distance-3d-fast ((part1 particle) (part2 particle))
  "Compute distance between two particles (without the sq. root)."
  (let ((x-diff (- (particle-loc-x part1) (particle-loc-x part2)))
        (y-diff (- (particle-loc-y part1) (particle-loc-y part2)))
        (z-diff (- (particle-loc-z part1) (particle-loc-z part2))))
    (+ (* x-diff x-diff) (* y-diff y-diff) (* z-diff z-diff))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
