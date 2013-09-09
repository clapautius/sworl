(in-package :sworl)

;; :debug:
(declaim (optimize debug))

(defclass u-window (glut:window)
  ((universe
    :initarg :universe
    :initform (error "Provide a universe to display")
    :reader universe)

   (draw-object
    :initarg :draw-object
    :initform (error "Provide a function to display objects")
    :accessor draw-object)
   
   ;; pause for every simulation step (in seconds)
   (pause
    :initarg :pause
    :initform nil ; sec.
    :accessor pause)

   (preferred-size
    :initarg :preferred-size
    :initform 100
    :accessor preferred-size)

   (trails
    :initarg :trails
    :initform nil
    :accessor trails)

   (axis-drawn
    :initform nil
    :accessor axis-drawn)
   
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


(defmethod glut:display-window :before ((w u-3d-window))
  (unless (opengl-initialized w)
    (let* ((u-size (preferred-size w))
           (camera-height (/ u-size 4))
           (camera-x-pos (/ u-size 2))
           (camera-y-pos (+ u-size (/ u-size 9)))
           (look-at-x 0)
           (look-at-y 0)
           (look-at-z 0))
      (gl:clear-color 0 0 0.2 0)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:viewport 0 0 u-size u-size)
      (glu:perspective 100 1 0 (* u-size 2))
      (glu:look-at camera-x-pos camera-y-pos camera-height ; camera pos
                   look-at-x look-at-y look-at-z  ; look at
                   0 0 1) ; up
      (setf (opengl-initialized w) t))))


(defun draw-axis (w)
  (let ((u-size (preferred-size w)))
    ;; axis
    (gl:color 0.2 0.2 0.2)
    (gl:with-primitive :lines
      (gl:vertex 0 0 0) (gl:vertex u-size 0 0)
      (gl:vertex 0 0 0) (gl:vertex 0 u-size 0)
      (gl:vertex 0 0 0) (gl:vertex 0 0 u-size))))


(defmethod glut:display ((w u-window))
  (when (not (trails w))
    (gl:clear :color-buffer))

  (if (trails w)
    (when (not (axis-drawn w))
      (draw-axis w)
      (setf (axis-drawn w) t))
    (draw-axis w))
  
  (dolist (obj (objects (universe w)))
    (funcall (draw-object w) obj))
  
  (glut:swap-buffers)
  
  (when (trails w)
    (dolist (obj (objects (universe w)))
      (funcall (draw-object w) obj t))))

  
(defmethod glut:idle ((w u-window))
  (and (pause w) (sleep (pause w)))
  (if (tick (universe w))
      (glut:post-redisplay)
      (glut:destroy-current-window)))
