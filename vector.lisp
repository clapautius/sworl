;;;; vector operations

(in-package :sworl)

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


(defun vector-substract (vect1 vect2)
  "Substract vect2 from vect1. Return a new vector."
  (let ((vect (make-array (length vect1))))
    (dotimes (i (length vect))
      (setf (aref vect i) (- (aref vect1 i) (aref vect2 i))))
    vect))


(defun vector-add (vect1 vect2)
  "Add vect1 and vect2. Return a new vector."
  (let ((vect (make-array (length vect1))))
    (dotimes (i (length vect))
      (setf (aref vect i) (+ (aref vect1 i) (aref vect2 i))))
    vect))


(defun vector-normalize (vect)
  "Normalize a vector."
  (vector-divide vect (vector-magnitude vect)))


(defun vector-from-to (location1 location2 magnitude)
  "Return a vector (as an array) from LOCATION1 to LOCATION2 having magnitude MAGNITUDE"
  (let* (;; components of the vector (from location1 to location2)
         (x (- (aref location2 0) (aref location1 0)))
         (y (- (aref location2 1) (aref location1 1)))
         (z (- (aref location2 2) (aref location1 2)))
         (result (make-array 3 :initial-contents (list x y z))))
    ;; normalize and then multiply with magnitude
    ;; :fixme: optimize this
    (vector-normalize result)
    (vector-multiply result magnitude)
    result))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
