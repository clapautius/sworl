(in-package :sworl.ants)


(defclass u-window (glut:window)
  ((universe
	:initarg :universe
	:initform (error "Provide a universe to display")
	:reader universe)

   ;; pause for every simulation step (in seconds)
   (pause
	:initarg :pause
	:initform nil ; sec.
	:accessor pause)

  (keep-trails
   :initarg :keep-trails
   :initform nil
   :reader keep-trails))

  (:default-initargs
   :title "Universe (sworl)"
	:mode '(:double :rgb)))

(defgeneric display-entity (window entity x y)
  (:documentation "Display the specified entity in the OpenGL window"))

(defmethod display-entity ((window u-window) (entity ant) x y)
  (let ((preferred-color (color entity)))
	(cond
	  ((eql preferred-color 'red)
	   (gl:color 1 0 0))
	  ((eql preferred-color 'blue)
	   (gl:color 0 0 1))
	  ((eql preferred-color 'green)
	   (gl:color 0 1 0))
	  ((eql preferred-color 'yellow)
	   (gl:color 1 1 0))
	  (t
	   (gl:color 1 1 1)))
	(gl:with-primitive :lines
	  (gl:vertex x y)
	  (gl:vertex (+ x 0.99) (+ y 0.99))
	  (gl:vertex x (+ y 0.99))
	  (gl:vertex (+ x 0.99) y))))


(defmethod display-static-element ((window u-window) st-elt x y)
  (when (typep st-elt 'static-element)
	(cond
	  ((equal st-elt 'rock)
	   (gl:color 0.5 0.5 0.5)
	   (gl:with-primitive :polygon
		 (gl:vertex x y)
		 (gl:vertex (+ x 0.99) y)
		 (gl:vertex  (+ x 0.99) (+ y 0.99))
		 (gl:vertex x (+ y 0.99))))
	  (t
	   (gl:color 1 1 0)
	   (gl:with-primitive :polygon
		 (gl:vertex x y)
		 (gl:vertex (+ x 0.99) y)
		 (gl:vertex  (+ x 0.99) (+ y 0.99))
		 (gl:vertex x (+ y 0.99)))))))


(defmethod glut:keyboard ((window u-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:display-window :before ((w u-window))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (glut:width w) 0 (glut:height w) -1 1))

(defmethod glut:display ((w u-window))
  (when (not (keep-trails w))
	(gl:clear :color-buffer))
  (dotimes (x (size (universe w)))
	(dotimes (y (size (universe w)))
	  (unless (null (aref (u-array (universe w)) x y))
		(display-entity w (aref (u-array (universe w)) x y) x y))
	  (unless (null (aref (u-array-static (universe w)) x y))
		(display-static-element w (aref (u-array-static (universe w)) x y) x y))))
  (glut:swap-buffers))

(defmethod glut:idle ((w u-window))
  (and (pause w) (sleep (pause w)))
  (if (passing-time-universal (universe w))
	  (glut:post-redisplay)
	  (glut:destroy-current-window)))
