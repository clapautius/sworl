(in-package :sworl)

;; :debug:
(declaim (optimize debug))


(defun display-object-as-polygon (obj)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         ;;(z (particle-loc-z obj))
         (xx1 (- x 1)) (xx2 (+ x 1))
         (yy1 (- y 1)) (yy2 (+ y 1)))
    ;; red color
    (gl:color 1 0 0)
    (gl:with-primitive :polygon
      (gl:vertex xx1 yy1) (gl:vertex xx2 yy1)
      (gl:vertex  xx2 yy2) (gl:vertex xx1 yy2))))


(defun display-object-as-polygon-big (obj)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         ;;(z (particle-loc-z obj))
         (xx1 (- x 100000)) (xx2 (+ x 100000))
         (yy1 (- y 100000)) (yy2 (+ y 100000)))
    ;; red color
    (gl:color 1 0 0)
    (gl:with-primitive :polygon
      (gl:vertex xx1 yy1) (gl:vertex xx2 yy1)
      (gl:vertex  xx2 yy2) (gl:vertex xx1 yy2))))


(defun sim-visual-simple-1 ()
  "Very simple simulation with one object."
  (let* ((universe (make-instance 'universe))
         (particle (make-instance 'particle :velocity (make-array 3 :initial-element 1))))
    (setf (objects universe) (cons particle (objects universe)))
    (glut:display-window (make-instance 'u-3d-window :width 200 :height 200
                                        :preferred-size 100
                                        :universe universe :pause 1
                                        :draw-object 'display-object-as-polygon))))


(defun sim-simple-1 ()
  "Very simple simulation with one object."
  (let* ((universe (make-instance 'universe))
         (particle (make-instance 'particle :velocity (make-array 3 :initial-element 1))))
    (setf (objects universe) (cons particle (objects universe)))
    (dotimes (i 100)
      (tick universe))))

(defun sim-visual-earth-satellite ()
  "Simple simulation with earth and one satellite (newtonian universe)."
  (let* ((universe (make-newton-universe))
         (earth (make-instance 'particle :mass 5.972E24))
         (satellite (make-instance 'particle :mass 1)))
    ;; set location and velocity for satellite
    (setf (aref (location satellite) 1) 6371000)
    (setf (aref (velocity satellite) 0) 8000)
  
    (setf (objects universe) (list earth))
    (setf (objects universe) (append (objects universe) (list satellite)))
    (dolist (obj (objects universe))
      (format t "~a~%" obj))
    (glut:display-window (make-instance 'u-3d-window :width 200 :height 200
                                        :preferred-size (* 2 6371100)
                                        :universe universe :pause 0.01
                                        :draw-object 'display-object-as-polygon-big))))
