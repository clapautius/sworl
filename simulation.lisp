(in-package :sworl)

;; :debug:
(declaim (optimize debug))

;;;; A universe = collection of objects and rules.
(defclass universe ()
  (
   ;; a list with objects in the universe
   (objects
    :initform nil
    :accessor objects)

   (rules-for-individuals
    :initform nil
    :accessor rules-one)

   (rules-for-pairs
    :initform nil
    :accessor rules-pair)

   ;; universal time
   (u-time
    :initform 0
    :accessor u-time))

  (:documentation "Universe = collection of objects and rules."))


(defmethod tick ((universe universe))
  "Update all objects and advance time with 1 time unit (one second)."
  ;; apply rules for individual objects
  (let ((rules (rules-one universe)))
    (when rules
      (dolist (obj (objects universe))
        (dolist (rule rules)
          (funcall rule obj)))))

  ;; apply rules for pairs of objects
  (let ((rules (rules-pair universe)))
    (when rules
      (do ((pos1 (objects universe) (cdr pos1)))
          ((null pos1))
        (dolist (obj2 (cdr pos1))
          (dolist (rule rules)
            (funcall rule (car pos1) obj2))))))

  ;; update objects
  (dolist (obj (objects universe))
    (update obj))

  (incf (u-time universe))

  (format t "Universe at time ~a~%" (u-time universe))
  (dolist (obj (objects universe))
    (format t "- ~a~%" obj))
  
  t)


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


(defmethod make-newton-universe ()
  "Create a universe containing Newton's universal law of gravity."
  (let ((newton-universe (make-instance 'universe)))
    (setf (rules-pair newton-universe)
          (cons 'newton-law-gravity-3d (rules-pair newton-universe)))
    newton-universe))
