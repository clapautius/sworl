(in-package :sworl.ants)

(declaim (optimize debug))

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


(defmacro dtorad (degrees)
  `(/ (* 3.1415926  ,degrees) 180))

(defmacro radtod (radians)
  `(/ (* ,radians 180) 3.1415926))


(deftype static-element () "Static elements in the universe." '(member rock))

(defclass universe ()
  ((size
    :initarg :size
    :initform (error "Universe MUST have a size!")
    :reader size)
   
   (u-array
    :initform nil
    :accessor u-array)

   (u-array-future
    :initform nil
    :accessor u-array-future)

   (u-array-static
    :initform nil
    :accessor u-array-static)
 
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

   (ant-move-func
    :initarg :ant-move-func
    :initform nil
    :reader ant-move-func)
   )

  (:documentation "An universe for ants"))


(defmethod initialize-instance :after ((universe universe) &key)
  (setf (slot-value universe 'u-array) (make-array
                                        (list (size universe) (size universe))
                                        :initial-element nil))
  (setf (slot-value universe 'u-array-future)
        (make-array (list (size universe) (size universe))
                    :initial-element nil))
  (setf (slot-value universe 'u-array-static)
        (make-array (list (size universe) (size universe))
                    :initial-element nil)))


(defgeneric empty (universe x y future)
  (:documentation "Check if the space is empty at the specified coordinates"))

(defgeneric place-elt-at (universe element x y &key future)
  (:documentation "Place a new element in the universe"))

(defgeneric place-static-element-at (universe static-element x y)
  (:documentation "Place a static element in the universe"))


(defmethod empty ((universe universe) x y future)
  (when (and (>= x 0) (>= y 0) (< x (size universe)) (< y (size universe)))
    (return-from empty
      (and (null (aref (u-array-static universe) x y))
           (if future
               (emptyp-universe-array (u-array-future universe) x y)
               (emptyp-universe-array (u-array universe) x y)))))
  nil)


(defun emptyp-universe-array (universe-array x y)
  "Return true if the specified array does not contain blocking elements at the
  specified coordinates"
  (or (null (aref universe-array x y))
      (eql (type-of (aref universe-array x y)) 'pheromone)))


(defmethod print-object ((universe universe) stream)
  (let ((u-name (list "static" "present" "future")))
    (dolist (universe-side (list (u-array-static universe) (u-array universe)
                                 (u-array-future universe)))
      (format stream "*** Side: ~a~%" (car u-name))
      (dotimes (x (size universe))
        (format stream "~2a|" x)
        (dotimes (y (size universe))
          (let ((elt (aref universe-side x y)))
            (cond
              ((or (null elt) (and (numberp elt) (zerop elt)))
               (format stream " "))
              ((eql (type-of elt) 'ANT)
               (format stream "A"))
              ((eql (type-of elt) 'pheromone)
               (format stream "."))
              ((typep elt 'static-element)
               (cond
                 ((equal elt 'rock)
                  (format stream "*"))
                 (t
                  (format stream "+"))))
              (t
               (format stream "?")))))
        (format stream "|")
        (terpri))
      (setf u-name (cdr u-name)))))


(defmethod place-static-element-at ((universe universe) element x y)
  (if (typep element 'static-element)
      (setf (aref (u-array-static universe) x y) element)
      (error "The element is not a static element")))
 

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

   (direction
    :documentation "The direction (angle in radians, values between 0 and 2*pi rad.)"
    :initarg :direction
    :initform 1.57 ; by default go up
    :accessor direction)

   (id
    :documentation "A unique identification string"
    :initarg :id
    :initform nil
    :reader id)

   ;; x-step & y-step are float values representing the exact value of a step on
   ;; x and y axis
   (x-step
    :reader x-step)
   (y-step
    :reader y-step)

   (preferred-color
    :initarg :color
    :initform 'red
    :reader color))
  
  (:documentation "Ant class"))


(defclass pheromone ()
  ((x-pos
    :initarg :x
    :initform 0
    :accessor x)
   (y-pos
    :initarg :y
    :initform 0
    :accessor y)

   (ph-type
    :documentation "Possible values: generic, food"
    :initarg :ph-type
    :initform 'generic
    :accessor ph-type)

   (age
    :initform 0
    :accessor age)
   (age-max
    :initform 10
    :initarg :age-max
    :accessor age-max)

   (intensity
    :initarg :intensity
    :initform 0
    :accessor intensity))

  (:documentation "Pheromone class"))


    
(defgeneric entity-change-dir (entity degrees)
  (:documentation "Change the entity direction with 'degrees' degrees"))

(defgeneric entity-try-move (ant universe x-step y-step)
  (:documentation "Try to move the specified entity in the universe. Return nil
  if not possible."))


;; initialize x-step & y-step 
;; fixme: - these values should be computed again if direction is modified
(defmethod initialize-instance :after ((ant ant) &key)
  (setf (slot-value ant 'x-step) (* (step-size ant) (cos (direction ant))))
  (setf (slot-value ant 'y-step) (* (step-size ant) (sin (direction ant)))))

(defmethod print-object ((ant ant) stream)
  (format stream "ANT(~a)[~a,~a]" (or (id ant) "noname") (x ant) (y ant)))


(defmethod entity-change-dir ((ant ant) rad)
  (setf (direction ant) (+ (direction ant) rad))
  (setf (slot-value ant 'x-step) (* (step-size ant) (cos (direction ant))))
  (setf (slot-value ant 'y-step) (* (step-size ant) (sin (direction ant))))
  ;;(ant-log 6 "step-size for ant is " (step-size ant))
  ;;(ant-log 6 "cos & sin for ant is " (cos (direction ant)) " , "
  ;;         (sin (direction ant)))
  (ant-log 4 "new direction for ant is " (direction ant))
  (ant-log 6 "new x-step & y-step for ant is " (slot-value ant 'x-step)
           (slot-value ant 'y-step))
  t)


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
  (ant-log 2 "time has passed for universe; time is now " (u-time universe))
  t)


(defmethod entity-try-move ((ant ant) (universe universe) x-step y-step)
  (let* ((x-new (+ (x ant) x-step))
         (y-new (+ (y ant) y-step))
         (x-new-int (round x-new))
         (y-new-int (round y-new)))
    (ant-log 5 "current params: x=" (x ant) " y=" (y ant) " x-step=" x-step " y-step=" y-step)
    (ant-log 4 "candidate positions for ant: " x-new "," y-new)
    ;; check free space
    (if (empty universe x-new-int y-new-int t)
        (progn
          (place-elt-at universe ant x-new-int y-new-int :future t)
          (setf (x ant) x-new)
          (setf (y ant) y-new)
          (ant-log 4 "new position for ant: " (x ant) "," (y ant))
          t)
        nil)))

(defun ant-try-move-forward (ant universe)
  ;; try to keep the same direction
  ;; x-new = x + cos(direction)
  ;; y-new = y + sin(direction)
  (entity-try-move ant universe (x-step ant) (y-step ant)))

(defun ant-try-move-left (ant universe)
  (and (entity-try-move ant universe (- (y-step ant)) (x-step ant))
       (entity-change-dir ant (dtorad 90))))

(defun ant-try-move-right (ant universe)
  (and (entity-try-move ant universe (y-step ant) (- (x-step ant)))
       (entity-change-dir ant (dtorad -90))))

(defun ant-try-move-backward (ant universe)
  (and (entity-try-move ant universe (- (x-step ant)) (- (y-step ant)))
       (entity-change-dir ant (dtorad 180))))

(defun ant-try-move-angle (ant universe angle)
  (ant-log 2 "  trying to change angle with " (radtod angle) " degrees")
  (let ((x-step (* (step-size ant) (cos (+ (direction ant) angle))))
        (y-step (* (step-size ant) (sin (+ (direction ant) angle)))))
    (and (entity-try-move ant universe x-step y-step)
         (entity-change-dir ant angle))))

(defun ant-move-deterministic (ant universe)
  (ant-log 2 "ant " ant " in action")
  (or (ant-try-move-forward ant universe)
      (ant-log 4 "cannot move forward")
      (ant-try-move-left ant universe)
      (ant-log 4 "cannot move left")
      (ant-try-move-right ant universe)
      (ant-log 4 "cannot move right")
      (ant-try-move-backward ant universe) 
      (ant-log 4 "cannot move backwards")
      (progn
        (ant-log 0 "ant is blocked and confused")
        t)))


(defun ant-move-random (ant universe &optional (angle-limit 70))
  (ant-log 2 "ant " ant " moving random")
  (if (plusp (random 3)) ; keep direction
      (progn
        (ant-log 2 "  trying to keep direction")
        (ant-move-deterministic ant universe))
      (progn
        (do ((angle (- (random (* angle-limit 2)) angle-limit))
             (retries 0))
            ((or (>= retries 4)
                 (ant-try-move-angle ant universe (dtorad angle)))
             (not (>= retries 4)))
          (incf retries)
          (setf angle (- (random (* angle-limit 2)) angle-limit))))))


(defun ant-move-random-or-det (ant universe)
  (or (ant-move-random ant universe)
      (ant-move-deterministic ant universe)))


(defmethod passing-time ((universe universe) (ant ant))
  (ant-log 2 "ant " ant " in action")
  (if (ant-move-func universe)
      (progn
        (ant-log 5 "calling custom func " (ant-move-func universe)
                 " for moving ant")
        (funcall (ant-move-func universe) ant universe))
      (ant-move-random-or-det ant universe)))


(defmethod place-elt-at ((universe universe) (ant ant) x y &key (future t))
  (let ((array (if future (u-array-future universe) (u-array universe)))
        (list (if future (dyn-elt-future universe) (dyn-elt-present universe))))
    (ant-log 3 "placing a new ant at " x "," y)
    (ant-log 3 "direction: " (radtod (direction ant)))
    (setf (aref array x y) ant)
    (if list
      (nconc list (list ant))
      (if future
          (setf (dyn-elt-future universe) (list ant))
          (setf (dyn-elt-present universe) (list ant))))))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
