;;;; Simulation of starling flocks.

;;; Max. speed of a starling (in the simulation): 1 m/s.

(in-package :sworl)

;; :debug:
(declaim (optimize debug))


(defparameter *starling-min-dist* 50)

(defclass starling (particle)
  ((closest-neighbor
    :accessor closest-neighbor)

   (shortest-dist
    :initform 0
    :accessor shortest-dist))

  (:documentation "Various parameters for starling."))


(defmethod print-object ((starling starling) stream)
  (format stream "starling: mass=~a, location=~a~%  velocity=~a, acceleration=~a~%
  shortest-dist=~a~%"
          (mass starling) (location starling)
          (velocity starling) (acceleration starling)
          (shortest-dist starling)))


(defun starling-goal (obj)
  "By default, the goal of a starling is to go towards greater X coord.
Force is max-desired-velocity - current-velocity."
  (let* ((desired-velocity (make-array 3 :initial-contents '(1 0 0)))
         (current-velocity (velocity obj))
         (steering-force (vector-substract desired-velocity current-velocity)))
    (apply-force steering-force obj)))


(defun starling-interaction (starling1 starling2)
  "Attraction and rejection rules."
  (format t ":debug: interaction between ~a and ~a~%" starling1 starling2)
  ;; select the closest starling and try to get nearer
  (let ((dist (distance-3d-fast starling1 starling2)))
    (when (or (zerop (shortest-dist starling1)) (< dist (shortest-dist starling1)))
      (setf (shortest-dist starling1) dist)
      (setf (closest-neighbor starling1) starling2))
    (when (or (zerop (shortest-dist starling2)) (< dist (shortest-dist starling2)))
      (setf (shortest-dist starling2) dist)
      (setf (closest-neighbor starling2) starling1))))


(defun starling-post-rule (starling)
  "Compute final direction"
  ;; try to get closer to the nearest neighbor
  (format t ":debug: starling (in post-rule): ~a~%" starling)
  (when (> (shortest-dist starling) *starling-min-dist*)
    (apply-force (vector-from-to (location starling)
                                 (location (closest-neighbor starling)) 0.5)
                 starling))
  ;; reset starling
  (setf (closest-neighbor starling) nil)
  (setf (shortest-dist starling) 0))

     
(defun make-starling-universe ()
  "Create a universe full of starlings."
  (let ((starling-universe (make-instance 'universe)))
    (setf (rules-one starling-universe)
          (cons 'starling-goal (rules-one starling-universe)))
    (setf (post-rules-one starling-universe)
          (cons 'starling-post-rule (post-rules-one starling-universe)))
    (setf (rules-pair starling-universe)
          (cons 'starling-interaction (rules-pair starling-universe)))
    starling-universe))


(defun run-starling-1 ()
  (let* ((universe (make-starling-universe))
         (starling1 (make-instance 'starling
                                   :location (make-array 3 :initial-contents '(0 100 0))))
         (starling2 (make-instance 'starling
                                   :location (make-array 3 :initial-contents '(0 150 0)))))
    (setf (objects universe) (cons starling1 (objects universe)))
    (setf (objects universe) (cons starling2 (objects universe)))
    (dotimes (i 100)
      (tick universe)
      (dolist (obj (objects universe))
        (format t "~a~%" obj)))))


(defun run-starling-2 ()
  (let* ((universe (make-starling-universe))
         (starling1 (make-instance 'starling
                                   :location (make-array 3 :initial-contents '(0 100 0))))
         (starling2 (make-instance 'starling
                                   :location (make-array 3 :initial-contents '(0 200 0)))))
    (setf (getf (appearance starling1) :shape) :square)
    (setf (getf (appearance starling2) :shape) :square)
    (setf (objects universe) (cons starling1 (objects universe)))
    (setf (objects universe) (cons starling2 (objects universe)))
    (glut:display-window (make-instance 'u-2d-window :width 1000 :height 600
                                        :universe universe :pause 0.01
                                        :grid 50))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
