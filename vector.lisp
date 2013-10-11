;;;; vector operations

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


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
