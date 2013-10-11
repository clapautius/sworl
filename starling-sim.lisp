;;;; Simulation of starling flocks.

;;; Max. speed of a starling (in the simulation): 1 m/s.

(in-package :sworl)

;; :debug:
(declaim (optimize debug))


(defun starling-goal (obj)
  "By default, the goal of a starling is to go towards greater X coord.
Force is max-desired-velocity - current-velocity."
  (let* ((desired-velocity (make-array 3 :initial-contents '(1 0 0)))
         (current-velocity (velocity obj))
         (steering-force (vector-substract desired-velocity current-velocity)))
    (apply-force steering-force obj)))



(defun make-starling-universe ()
  "Create a universe full of starlings."
  (let ((starling-universe (make-instance 'universe)))
    (setf (rules-one starling-universe)
          (cons 'starling-goal (rules-one starling-universe)))
    starling-universe))


(defun run-starling-sim-1 ()
  (let* ((universe (make-starling-universe))
         (starling (make-instance 'particle)))
    (setf (objects universe) (cons starling (objects universe)))
    (dotimes (i 100)
      (tick universe)
      (dolist (obj (objects universe))
        (format t "~a~%" obj)))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
