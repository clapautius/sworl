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


(defun display-object-as-polygon-big (obj &optional trail)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         (z (particle-loc-z obj))
         (xx1 (- x 100000)) (xx2 (+ x 100000))
         (yy1 (- y 100000)) (yy2 (+ y 100000)))
    (if trail
        ;; red color
        (gl:color 0.1 0 0)
        ;; red color
        (gl:color 1 0 0))
    (gl:push-matrix)
    (gl:translate x y z)
    (glu:sphere (glu:new-quadric) 100000 10 10)
    (gl:pop-matrix)
    (gl:with-primitive :polygon
      (gl:vertex xx1 yy1) (gl:vertex xx2 yy1)
      (gl:vertex  xx2 yy2) (gl:vertex xx1 yy2))))


(defun display-object-as-sphere-big (obj &optional trail)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         (z (particle-loc-z obj)))
    (if trail
        ;; red color
        (gl:color 0.1 0 0)
        ;; red color
        (gl:color 1 0 0))
    (gl:push-matrix)
    (gl:translate x y z)
    (glu:sphere (glu:new-quadric) 100000 10 10)
    (gl:pop-matrix)))


(defun display-object-as-sphere (obj &optional trail)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         (z (particle-loc-z obj)))
    (if trail
        ;; red color
        (gl:color 0.1 0 0)
        ;; red color
        (gl:color 1 0 0))
    (gl:push-matrix)
    (gl:translate x y z)
    (glu:sphere (glu:new-quadric) 100 10 10)
    (gl:pop-matrix)))


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
    (setf (aref (location satellite) 2) 6371000)
    (setf (aref (velocity satellite) 0) 2000)
  
    (setf (objects universe) (list earth))
    (setf (objects universe) (append (objects universe) (list satellite)))
    (dolist (obj (objects universe))
      (format t "~a~%" obj))
    (glut:display-window (make-instance 'u-3d-window :width 200 :height 200
                                        :preferred-size (* 2 6371100)
                                        :trails t
                                        :universe universe :pause 0.01
                                        :draw-object 'display-object-as-sphere-big))))


(defun sim-visual-cube ()
  "Simple simulation with particles in a cube (newtonian universe)."
  (let* ((universe (make-newton-universe))
         (p1 (make-instance 'particle :mass 1e16
                            :location (make-array 3 :initial-contents '(0 0 0))
                            :velocity (make-array 3 :initial-contents '(10 0 0))))
         (p2 (make-instance 'particle :mass 2e16
                            :location (make-array 3 :initial-contents '(10000 0 0))
                            :velocity (make-array 3 :initial-contents '(0 10 0))))
         (p3 (make-instance 'particle :mass 1e16
                            :location (make-array 3 :initial-contents '(10000 5000 5000))
                            :velocity (make-array 3 :initial-contents '(-5 -5 0))))
         (p4 (make-instance 'particle :mass 6e15
                            :location (make-array 3 :initial-contents '(0 8000 5000))))
         (p5 (make-instance 'particle :mass 1e16
                            :location (make-array 3 :initial-contents '(2000 2000 2000))))
         (p6 (make-instance 'particle :mass 1e16
                            :location (make-array 3 :initial-contents '(6000 6000 5000))
                            :velocity (make-array 3 :initial-contents '(-1 -2 -3))))
         )

    (setf (objects universe) (list p1))
    (setf (objects universe) (append (objects universe) (list p2)))
    (setf (objects universe) (append (objects universe) (list p3)))
    (setf (objects universe) (append (objects universe) (list p4)))
    (setf (objects universe) (append (objects universe) (list p5)))
    (setf (objects universe) (append (objects universe) (list p6)))
    (glut:display-window (make-instance 'u-3d-window :width 1200 :height 750
                                        :preferred-size 20000
                                        :trails t
                                        :universe universe :pause 0.01
                                        :draw-object 'display-object-as-sphere))))


(defun sim-visual-1 ()
  "Simple simulation with particles (newtonian universe)."
  (let* ((universe (make-newton-universe))
         (p1 (make-instance 'particle :mass 2e16
                            :location (make-array 3 :initial-contents '(0 0 4000))
                            :velocity (make-array 3 :initial-contents '(3 3 0))))
         (p2 (make-instance 'particle :mass 2e16
                            :location (make-array 3 :initial-contents '(0 0 -4000))
                            :velocity (make-array 3 :initial-contents '(3 -3 0))))
         (p3 (make-instance 'particle :mass 2.5e16
                            :location (make-array 3 :initial-contents '(-3000 0 0))
                            :velocity (make-array 3 :initial-contents '(0 0 0))))
         )

    (setf (objects universe) (list p1))
    (setf (objects universe) (append (objects universe) (list p2)))
    (setf (objects universe) (append (objects universe) (list p3)))
    (glut:display-window (make-instance 'u-3d-window :width 1200 :height 750
                                        :preferred-size 20000
                                        :trails t
                                        :universe universe :pause 0.01
                                        :draw-object 'display-object-as-sphere))))
