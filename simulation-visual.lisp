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
    :initform nil
    :accessor draw-object)
   
   ;; pause for every simulation step (in seconds)
   (pause
    :initarg :pause
    :initform nil ; sec.
    :accessor pause)

   (png-file-name
    :initarg :png-file-name
    :initform nil
    :accessor png-file-name)
   
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

   (color
    :initarg :color
    :initform '(0.695 0.901 1)
    :accessor color)

   (light
    :initarg :light
    :initform nil
    :accessor light)

   ;; quadratic attenuation for light
   (light-att-q
    :initarg :light-att-q
    :initform nil
    :accessor light-att-q)

   (theme
    :initarg :theme
    :initform nil
    :accessor theme)
   
   ;; :todo: - find the right way to do this
   (opengl-initialized
    :initform nil
    :accessor opengl-initialized))

  (:default-initargs
   :title "Universe (sworl)"
    :mode '(:double :rgb)))


(defclass u-3d-window (u-window)
  ((camera-x-pos
    :initarg :camera-x-pos
    :accessor camera-x-pos)
   (camera-y-pos
    :initarg :camera-y-pos
    :accessor camera-y-pos)
   (camera-z-pos
    :initarg :camera-z-pos
    :accessor camera-z-pos)
   (look-at
    :initarg :look-at
    :initform '(0 0 0)
    :accessor look-at))
  (:documentation "Class for OpenGL window with 3D graphics"))


(defmethod glut:keyboard ((window u-window) key x y)
  (declare (ignore x y))
  (when (eql key #\Esc)
    (glut:destroy-current-window)))


(defun glut-prepare-display-common (w)
  "Common operation to prepare the OpenGL window"
  (cond
    ((eql (theme w) :dark-with-floor)
     ;(setf (color w) (list 0.098 0.243 0.458))
     (setf (color w) (list 0.058 0.203 0.418))
     ))
  (gl:clear-color (first (color w)) (second (color w)) (third (color w)) 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)

  ;; setup lighting
  (when (light w)
    (gl:enable :lighting)
    (gl:enable :light0)
    (gl:enable :color-material)
    (gl:light :light0 :position (light w))
    (gl:light :light0 :constant-attenuation 0)
    ;;(gl:light :light0 :linear-attenuation 0.0003)
    (gl:light :light0 :linear-attenuation 0)
    (gl:light :light0 :quadratic-attenuation (or (light-att-q w) 0.0000000008))
    ))


(defmethod glut:display-window :before ((w u-window))
  (unless (opengl-initialized w)
    (glut-prepare-display-common w)
    (gl:ortho 0 (glut:width w) 0 (glut:height w) -1 1)
    (setf (opengl-initialized w) t)))


(defmethod glut:display-window :before ((w u-3d-window))
  (unless (opengl-initialized w)
    (let* ((u-size (preferred-size w))
           (camera-height (or (camera-z-pos w) (/ u-size 4)))
           (camera-x-pos (or (camera-x-pos w) (/ u-size 2)))
           (camera-y-pos (or (camera-y-pos w) (+ u-size (/ u-size 9))))
           (look-at-x (first (look-at w)))
           (look-at-y (second (look-at w)))
           (look-at-z (third (look-at w))))
      (glut-prepare-display-common w)
      (gl:viewport 0 0 u-size u-size)
      (glu:perspective 100 1 0 (* u-size 2))
      (glu:look-at camera-x-pos camera-y-pos camera-height ; camera pos
                   look-at-x look-at-y look-at-z  ; look at
                   0 0 1) ; up
      (setf (opengl-initialized w) t))))


(defun sim-draw-axis (w)
  (let* ((u-size (preferred-size w))
         (u-size-half (/ u-size 2))
         (height (- (/ u-size 6))))
    (cond
      ((eql (theme w) :dark-with-floor)
       (gl:color 0.1 0.2 0.5)
       (gl:with-primitive :polygon
         (gl:normal 0 0 1)
         (gl:vertex (- u-size-half) (- u-size-half) height)
         (gl:normal 0 0 1)
         (gl:vertex u-size-half (- u-size-half) height)
         (gl:normal 0 0 1)
         (gl:vertex u-size-half u-size-half height)
         (gl:normal 0 0 1)
         (gl:vertex (- u-size-half) u-size-half height)))
      (t
       (gl:color 0.2 0.2 0.2)
       (gl:with-primitive :lines
         (gl:vertex 0 0 0) (gl:vertex u-size 0 0)
         (gl:vertex 0 0 0) (gl:vertex 0 u-size 0)
         (gl:vertex 0 0 0) (gl:vertex 0 0 u-size))))))


(defun sim-draw-object (obj &optional trail)
  (let* ((x (particle-loc-x obj))
         (y (particle-loc-y obj))
         (z (particle-loc-z obj))
         (shape (getf (appearance obj) :shape :sphere))
         (size (getf (appearance obj) :size 10))
         (color (getf (appearance obj) :color '(1 0 0)))
         (trail-color (getf (appearance obj) :trail-color '(0.1 0.1 0.1))))
    (cond
      ((eq shape :sphere)
       (if trail
           (gl:color (first trail-color) (second trail-color) (third trail-color))
           (gl:color (first color) (second color) (third color)))
       (gl:push-matrix)
       (gl:translate x y z)
       ;(glu:sphere (glu:new-quadric) size 10 10)
       (glu:sphere (glu:new-quadric) size 22 22)
       (gl:pop-matrix))
      (t
       (error "Not implemented yet")))))


(defun sim-write-to-png (open-gl-data width height png-file-name)
  (let* ((png (make-instance 'zpng:png :color-type :truecolor
                             :width width :height height))
         (png-array (zpng:data-array png)))
    (loop for x from 0 to (1- width) do
       ;;(format t "x=~a~%" x)
         (loop for y from 0 to (1- height) do
              (let ((png-h  (- height y 1))
                    (open-gl-pos (+ (* y width 3) (* x 3))))
                (setf (aref png-array png-h x 0)
                      (aref open-gl-data open-gl-pos))
                (setf (aref png-array png-h x 1)
                      (aref open-gl-data (+ open-gl-pos 1)))
                (setf (aref png-array png-h x 2)
                      (aref open-gl-data (+ open-gl-pos 2))))))
    (format t "writing png ~a~%" png-file-name)
    (zpng:write-png png png-file-name)))


(defun sim-export-to-png (window png-file-name)
  "Export image from opengl WINDOW to PNG-FILE-NAME."
  (declare (ignore window))

  ;; Using gl:read-pixels takes a veeery long time (12 sec / image).
  ;; (let* ((width (glut:width window))
  ;;        (height (glut:height window))
  ;;        open-gl-data)
  ;;   (time (setf open-gl-data (gl:read-pixels 0 0 width height :rgb :unsigned-byte)))
  ;;   (time (sim-write-to-png open-gl-data width height png-file-name)))

  ;; :kluge: - use xwd and convert
  (let ((proc-ret (sb-ext:run-program "xwd" (list
                                             "-out"
                                             (concatenate 'string png-file-name ".xwd")
                                             "-nobdrs"
                                             "-name"
                                             "Universe (sworl)"
                                             "-silent")
                                      :search t)))
    (when (= (sb-ext:process-exit-code proc-ret) 0)
      (let ((proc2-ret (sb-ext:run-program "convert"
                                           (list
                                            (concatenate 'string png-file-name ".xwd")
                                            png-file-name)
                                           :search t)))
        ;; ignore exit code for rm
        (sb-ext:run-program "rm"
                            (list (concatenate 'string png-file-name ".xwd"))
                            :search t)
        (when (= (sb-ext:process-exit-code proc2-ret) 0)
          (return-from sim-export-to-png t)))))
  nil)


(defmethod glut:display ((w u-window))
  (when (not (trails w))
    (gl:clear :color-buffer))

  (if (trails w)
    (when (not (axis-drawn w))
      (sim-draw-axis w)
      (setf (axis-drawn w) t))
    (sim-draw-axis w))
  
  (dolist (obj (objects (universe w)))
    (let ((draw-func (draw-object w)))
      (if draw-func
          (funcall draw-func obj)
          (sim-draw-object obj))))
  
  (glut:swap-buffers)

  (when (png-file-name w)
    (let ((png-file-name-num ; png filename with number
           (format nil "~a~5,'0d.png" (png-file-name w) (u-time (universe w)))))
      (when (not (sim-export-to-png w png-file-name-num))
        (format t "Error: error exporting to png file ~a~%" png-file-name-num))))
  
  (when (trails w)
    (dolist (obj (objects (universe w)))
      (let ((draw-func (draw-object w)))
        (if draw-func
            (funcall (draw-object w) obj t)
            (sim-draw-object obj t))))))

  
(defmethod glut:idle ((w u-window))
  (and (pause w) (sleep (pause w)))
  (if (tick (universe w))
      (glut:post-redisplay)
      (glut:destroy-current-window)))
