(in-package :sworl.ants)


(defclass u-window (glut:window)
  ((universe
	:initarg :universe
	:initform (error "Provide a universe to display")
	:reader universe))

  (:default-initargs
   :title "Universe (sworl)"
	:mode '(:double :rgb)))

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
  (dotimes (x (size (universe w)))
	(dotimes (y (size (universe w)))
	  (unless (null (aref (u-array (universe w)) x y))
		(gl:color 1 0 0)
		(gl:with-primitive :points
		  (gl:vertex x y)
		  ;(gl:vertex (+ x 2) y)
		  ;(gl:vertex (+ x 2) (+ y 2))
		  ;(gl:vertex x (+ y 2))
		  ))))
  (glut:swap-buffers))

(defmethod glut:idle ((w u-window))
  ;(sleep 0.5)
  (if (passing-time-universal (universe w))
	  (glut:post-redisplay)
	  (glut:destroy-current-window)))
