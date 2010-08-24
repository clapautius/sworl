(in-package :sworl.ants)

(declaim (optimize debug))


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
   :reader keep-trails)

   ;; :todo: - find the right way to do this
   (opengl-initialized
	:initform nil
	:accessor opengl-initialized))

  (:default-initargs
   :title "Universe (sworl)"
	:mode '(:double :rgb)))


(defclass u-3d-window (u-window)
  ()
  (:documentation "Class for OpenGL window with 3D graphics"))


(defgeneric display-entity (window entity x y)
  (:documentation "Display the specified entity in the OpenGL window"))

(defgeneric display-entity-list (window ent-list x y)
  (:documentation "Display every entity at coords. x,y"))


(defmethod display-entity-list ((window u-window) list x y)
  "Display every element from the list (located at coord. x,y)"
  ;; first draw the pheromones, than the ants
  (dolist (elt (sort list (lambda (x y) (and (eql (type-of x) 'pheromone)
											 (eql (type-of y) 'ant)))))
	(display-entity window elt x y)))


(defmethod display-entity ((window u-window) (entity ant) x y)
  ;;(break)
  (let ((preferred-color (color entity)))
	(case preferred-color
	  (red (gl:color 1 0 0))
	  (blue (gl:color 0 0 1))
	  (green (gl:color 0 1 0))
	  (yellow (gl:color 1 1 0))
	  (cyan (gl:color 0 1 1))
	  (magenta (gl:color 1 0 1))
	  (otherwise (gl:color 1 0.5 0.5)))
	(gl:with-primitive :polygon
	  (gl:vertex x y) (gl:vertex (+ x 0.99) y)
	  (gl:vertex  (+ x 0.99) (+ y 0.99)) (gl:vertex x (+ y 0.99)))))
	;;(gl:with-primitive :lines
	  ;;(gl:vertex x y)
	  ;;(gl:vertex (+ x 0.99) (+ y 0.99))
	  ;;(gl:vertex x (+ y 0.99))
	  ;;(gl:vertex (+ x 0.99) y))))


(defmethod display-entity ((window u-window) (entity pheromone) x y)
  ;;(break)
  (cond
	((eql (ph-type entity) 'generic)
	 (let ((grey (/ (intensity entity) 10)))
	   (gl:color grey grey grey)
	   (gl:with-primitive :polygon
		 (gl:vertex x y) (gl:vertex (+ x 0.99) y)
		 (gl:vertex  (+ x 0.99) (+ y 0.99)) (gl:vertex x (+ y 0.99)))))))
	   

(defmethod display-static-element ((window u-window) st-elt x y)
  (when (typep st-elt 'static-element)
	(cond
	  ((equal st-elt 'rock)
	   (gl:color 1 0.5 0.5)
	   (gl:with-primitive :polygon
		 (gl:vertex x y) (gl:vertex (+ x 0.99) y)
		 (gl:vertex  (+ x 0.99) (+ y 0.99)) (gl:vertex x (+ y 0.99))))
	  ;;(t
	   ;;(gl:color 1 1 0)
	   ;;(gl:with-primitive :polygon
		 ;;(gl:vertex x y) (gl:vertex (+ x 0.99) y) 
		 ;;(gl:vertex  (+ x 0.99) (+ y 0.99)) (gl:vertex x (+ y 0.99))))
	  )))


(defmethod glut:keyboard ((window u-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))

(defmethod glut:display-window :before ((w u-window))
  (unless (opengl-initialized w)
	(gl:clear-color 0 0.1 0 0)
	(gl:matrix-mode :projection)
	(gl:load-identity)
	(gl:ortho 0 (glut:width w) 0 (glut:height w) -1 1)
	(setf (opengl-initialized w) t)))

(defmethod glut:display ((w u-window))
  (when (not (keep-trails w))
	(gl:clear :color-buffer))

  (let ((u-size (size (universe w))))
	;; axis
	(gl:color 1 0 0)
	(gl:with-primitive :lines
	  (gl:vertex 0 0 0) (gl:vertex (+ u-size 10) 0 0)
	  (gl:vertex 0 0 0) (gl:vertex 0 (+ u-size 10) 0)
	  (gl:vertex 0 0 0) (gl:vertex 0 0 10))

	;; earth
	(gl:color 0.1 0.8 0.1)
	(gl:with-primitive :polygon
	  (gl:vertex 0 0) (gl:vertex u-size 0)
	  (gl:vertex  u-size u-size) (gl:vertex 0 u-size)))

  (dotimes (x (size (universe w)))
	(dotimes (y (size (universe w)))
	  (when (aref (u-array (universe w)) x y)
		(display-entity-list w (aref (u-array (universe w)) x y) x y))
	  (when (aref (u-array-static (universe w)) x y)
		(display-static-element w (aref (u-array-static (universe w)) x y) x y))))
  (glut:swap-buffers))

(defmethod glut:idle ((w u-window))
  (and (pause w) (sleep (pause w)))
  (if (passing-time-universal (universe w))
	  (glut:post-redisplay)
	  (glut:destroy-current-window)))


(defmethod glut:display-window :before ((w u-3d-window))
  (unless (opengl-initialized w)
	(let ((u-size (size (universe w)))
		  (camera-height 100)
		  (camera-y-offset 100))
	  (gl:clear-color 0.4 0.4 0.8 0)
	  (gl:matrix-mode :projection)
	  (gl:load-identity)
	  (gl:viewport 0 0 u-size u-size)
	  (glu:perspective 145 1 0 (/ u-size 2))
	  (glu:look-at (/ u-size 2) camera-y-offset camera-height ; camera pos
				   (/ u-size 2) (/ u-size 4) 0  ; look at
				   0 0 1) ; up
	  (setf (opengl-initialized w) t))))
