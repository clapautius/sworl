(in-package :sworl.ants)


;;; logging
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ant-log-level* 5))

(defmacro ant-log (log-level &rest print-list)
  (if (<= log-level *ant-log-level*)
      `(progn
         ;; :fixme: replace format with something else
         ,(when (plusp log-level)
                `(dotimes (i ,log-level) (format t " ")))
         ,(when (>= log-level 3)
                `(format t ":debug:"))
         (let ((eol t))
           (dolist (p (list ,@print-list))
             (if (eql p ':no-eol)
                 (setf eol nil)
                 (format t "~a" p)))
           (when eol
             (terpri))))))

;;; end logging stuff


(defclass universe ()
  ((size
    :initarg :size
    :initform (error "Universe MUST have a size")
    :reader size)
   
   (u-array
    :initform nil
    :accessor u-array)

   (u-array-future
    :initform nil
    :accessor u-array-future)

   (u-time
    :initform 0
    :accessor u-time)


   (u-max-age
    :initarg :max-age
    :initform nil ; nil = unlimited
    :accessor u-max-age)

  (dynamic-elements-future
   :initform nil
   :accessor dyn-elt-future)

  (dynamic-elements-present
   :initform nil
   :accessor dyn-elt-present)
   )

  (:documentation "An universe for ants"))


(defmethod initialize-instance :after ((universe universe) &key)
  (setf (slot-value universe 'u-array) (make-array
                                        (list (size universe) (size universe))
                                        :initial-element nil))
  (setf (slot-value universe 'u-array-future)
        (make-array (list (size universe) (size universe))
                    :initial-element nil)))


(defgeneric empty (universe x y future)
  (:documentation "Check if the space is empty at the specified coordinates"))

(defgeneric place-element-at (universe element x y &key future)
  (:documentation "Place a new element in the universe"))


(defmethod empty ((universe universe) x y future)
  (when (and (>= x 0) (>= y 0) (< x (size universe)) (< y (size universe)))
    (return-from empty
      (if future
          (null (aref (u-array-future universe) x y))
          (null (aref (u-array universe) x y)))))
  nil)


(defmethod print-object ((universe universe) stream)
  (dotimes (x (size universe))
    (dotimes (y (size universe))
      (let ((elt (aref (u-array universe) x y)))
        (cond
          ((or (null elt) (zerop elt))
           (format stream "     "))
          ((eql (type-of elt) 'ANT)
           (format stream " ANT "))
          (t
           (format stream "  ?  ")))))
    (terpri)))


(defclass ant ()
  ((x-pos
    :initarg :x
    :initform 0
    :accessor x)
   (y-pos
    :initarg :y
    :initform 0
    :accessor y)

   (step-size
    :initarg :step-size
    :initform 1
    :accessor step-size)

   ;; by default go up
   (x-direction
    :initarg :x-dir
    :initform 0
    :accessor x-dir)
   (y-direction
    :initarg :y-dir
    :initform 1
    :accessor y-dir)

  ;; x-step & y-step are float values representing the exact value of a step on
  ;; x and y axis
  (x-step
   :reader x-step)
  (y-step
   :reader y-step))

  (:documentation "Ant class"))


;; initialize x-step & y-step
;; :fixme: - these values should be computed again if x-direction or y-direction
;; is modified
(defmethod initialize-instance :after ((ant ant) &key)
  (let* ((arctn (atan (y-dir ant) (x-dir ant))))
    (setf (slot-value ant 'x-step) (* (step-size ant) (cos arctn)))
    (setf (slot-value ant 'y-step) (* (step-size ant) (sin arctn)))))

(defmethod print-object ((ant ant) stream)
  (format stream "ANT(~a,~a)" (x ant) (y ant)))


;;; passing-time multimethod
(defgeneric passing-time-universal (universe)
  (:documentation "Passing time for universe, return nil if the universe has
reached its maximum age or some other cataclysm has happened."))

(defgeneric passing-time (universe element)
  (:documentation "Passing time for an element from the universe"))

(defun clear-array-fast (array no-of-elements)
  (let ((fast-array (make-array (list no-of-elements)
                                :displaced-to array)))
    (fill fast-array nil)))

(defmethod passing-time-universal ((universe universe))  
  (ant-log 2 "preparing for next moment in time: " (u-time universe))

  (when (and (u-max-age universe) (>= (u-time universe) (u-max-age universe)))
    (ant-log 1 "Universe has reached its max age. It's the end of the world")
    (return-from passing-time-universal nil))
    

  (dolist (elt (dyn-elt-present universe))
        (passing-time universe elt))

  (incf (u-time universe))
  (let ((old-array (u-array universe)))
    (setf (u-array universe) (u-array-future universe))
    (setf (u-array-future universe) old-array))
  (setf (dyn-elt-present universe) (dyn-elt-future universe))
  (setf (dyn-elt-future universe) nil)
  (clear-array-fast (u-array-future universe) (* (size universe) (size universe)))
  (ant-log 2 "time has passed for universe; time is now " (u-time universe)))


(defmethod ant-try-move ((ant ant) (universe universe) x-step y-step)
  (let* ((x-new (+ (x ant) x-step))
         (y-new (+ (y ant) y-step))
         (x-new-int (round x-new))
         (y-new-int (round y-new)))
    (ant-log 5 "current params: x=" (x ant) " y=" (y ant) " x-step=" x-step " y-step=" y-step)
    (ant-log 4 "candidate positions for ant: " x-new "," y-new)
    ;; check free space
    (if (empty universe x-new-int y-new-int t)
        (progn
          (place-element-at universe ant x-new-int y-new-int :future t)
          (setf (x ant) x-new)
          (setf (y ant) y-new)
          (ant-log 2 "new position for ant: " (x ant) "," (y ant))
          t)
        (progn
          (ant-log 2 "ant cannot go forward")
          nil))))


(defun ant-try-move-forward (ant universe)
  ;; try to keep the same direction
  ;; x-new = x + cos(arctan(y-dir/x-dir))
  ;; y-new = y + sin(arctan(y-dir/x-dir))
  (ant-try-move ant universe (x-step ant) (y-step ant)))


(defun ant-try-move-left (ant universe)
  (ant-try-move ant universe (- (y-step ant)) (x-step ant)))

(defun ant-try-move-right (ant universe)
  (ant-try-move ant universe (y-step ant) (- (x-step ant))))

(defun ant-try-move-backward (ant universe)
  (ant-try-move ant universe (- (x-step ant)) (- (y-step ant))))


(defmethod passing-time ((universe universe) (ant ant))
  (ant-log 2 "ant " ant " in action")
  (or (ant-try-move-forward ant universe)
      (ant-try-move-left ant universe)
      (ant-try-move-right ant universe)
      (ant-try-move-backward ant universe) 
      (progn
        (ant-log 0 "ant is blocked and confused")
        t)))


(defmethod place-element-at ((universe universe) (ant ant) x y &key (future t))
  (let ((array (if future (u-array-future universe) (u-array universe)))
        (list (if future (dyn-elt-future universe) (dyn-elt-present universe))))
    (setf (aref array x y) ant)
    (if list
      (nconc list (list ant))
      (if future
          (setf (dyn-elt-future universe) (list ant))
          (setf (dyn-elt-present universe) (list ant))))))


;;; testing functions
(defun generate-ants (number universe &optional (random-direction nil))
  "Generate <number> ants and place them in the universe"
  (flet ((get-free-space (universe)
           "Find a free space in the universe (return multiple values)"
           (dotimes (x (size universe))
             (dotimes (y (size universe))
               (if (empty universe x y nil)
                   (progn
                     (ant-log 5 "found an empty spot at " x "," y)
                     (return-from get-free-space (values x y)))
                   (ant-log 5 "spot at " x "," y " not empty"))))
           (values nil nil)))
    (let (x-empty y-empty)
      (dotimes (i number)
        (multiple-value-setq (x-empty y-empty)
          (get-free-space universe))
        (when (null x-empty)
          (error "Universe is full"))
        (place-element-at universe
                          (if random-direction
                              (make-instance 'ant :x x-empty :y y-empty
                                             :x-dir (random 10)
                                             :y-dir (random 10))
                              (make-instance 'ant :x x-empty :y y-empty))
                          x-empty y-empty :future nil)
        (ant-log 3 "placing a new ant at " x-empty "," y-empty)
        (ant-log 3 "direction: "
                 (x-dir (aref (u-array universe) x-empty y-empty))
                 ","
                 (y-dir (aref (u-array universe) x-empty y-empty)))))))


(defun run (&key (size 500) (ants 10) (duration 100))
  "Run the simulation"
  (ant-log 0 "Run a simulation with " ants " ants in an universe of size " size)
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((universe (make-instance 'universe :size size)))
    (generate-ants ants universe t)
    (dotimes (i duration)
      (passing-time-universal universe)))
  (ant-log 0 "It's the end of the world"))


(defun run-opengl (&key (size 500) (ants 10) (duration 100))
  "Run the simulation"
  (ant-log 0 "Run a simulation with " ants " ants in an universe of size " size)
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((universe (make-instance 'universe :size size :max-age duration)))
    (generate-ants ants universe t)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :universe universe)))
  (ant-log 0 "That's all folks!"))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
