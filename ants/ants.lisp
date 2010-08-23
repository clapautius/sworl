(in-package :sworl.ants)

(declaim (optimize debug))

;;; logging
;;; 0 - errors (always)
;;; 1 - warnings
;;; 2 - info low
;;; 3 - info medium (entering / exiting functions)
;;; 4 - etc
;;; 5 - objects
;;; 6 - objects frequently used
;;; 7 - usually temp stuff (probably not used after implementation)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ant-log-level* 4))

(defparameter *pheromone-max-intensity* 10)
(defparameter *ant-history-len* 31)
(defparameter *ant-smell-radius* 3)

(defparameter *around-list* (list (cons -1  -1) (cons  0 -1) (cons  1 -1)
                                  (cons -1   0) (cons  1  0) (cons -1  1)
                                  (cons  0   1) (cons  1  1)))


(defmacro ant-log (log-level &rest print-list)
  (if (<= log-level *ant-log-level*)
      `(progn
         ;; :fixme: replace format with something else
         ,(when (plusp log-level)
                `(dotimes (i ,log-level) (format t " ")))
         ,(when (>= log-level 3)
                `(format t ":debug: "))
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


(defun distance-manhattan (x1 y1 x2 y2)
  "Calculate distance between coordinates (manhattan distance, for speed)"
  (+ (abs (- x1 x2)) (abs (- y1 y2))))


(defun coords-in-list (x y list)
  (some (lambda (p) (equal (cons x y) p)) list))


(defun min-max-with-pos (list)
  (let ((min (first list)) (max (first list))
        (min-pos 0) (max-pos 0)
        (pos 0))
    (dolist (elt list)
      (when (not (eq elt (first list)))
        (when (< elt min)
          (setf min elt)
          (setf min-pos pos))
        (when (> elt max)
          (setf max elt)
          (setf max-pos pos)))
      (incf pos))
    (values-list (list min min-pos max max-pos))))


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

   (ant-move-func ; :todo: rename to ant-move-fn
    :initarg :ant-move-func
    :initform nil
    :reader ant-move-func))

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


(defgeneric empty-p (universe x y future)
  (:documentation "Check if the space is empty at the specified coordinates"))

(defgeneric place-elt-at (universe element x y &key future)
  (:documentation "Place a new element in the universe"))

(defgeneric get-elt-at (universe element-type x y &key future)
  (:documentation "Get the (first) element of type 'element-type' at coords. x,y"))

(defgeneric delete-elt-at (universe element-type x y &key future)
  (:documentation "Remove the specified element from universe"))

(defgeneric place-static-elt-at (universe static-element x y)
  (:documentation "Place a static element in the universe"))


(defmethod empty-p ((universe universe) x y future)
  "Return true if the specified array does not contain blocking elements at the
  specified coordinates"
  (when (and (>= x 0) (>= y 0) (< x (size universe)) (< y (size universe)))
    (let ((array (if future (u-array-future universe) (u-array universe))))
      (return-from empty-p
        (and (null (aref (u-array-static universe) x y))
             (every 'empty-element-p (aref array x y))))))
  nil)


(defun empty-element-p (elt)
  (or (null elt)
      (eql (type-of elt) 'pheromone)))


(defun print-elements (elements stream)
  (cond
    ((null elements)
     (format stream " "))
    ((typep elements 'static-element)
     (cond
       ((equal elements 'rock)
        (format stream "R"))
       ;;(t
       ;;(format stream "+"))
       ))
    ((eql (length elements) 1)
     (let ((elt (first elements)))
       (cond
         ((or (null elt) (and (numberp elt) (zerop elt)))
          (format stream " "))
         ((eql (type-of elt) 'ANT)
          (format stream "A"))
         ((eql (type-of elt) 'pheromone)
          (format stream "."))
         (t
          (format stream "?")))))
    (t
     (format stream "*"))))


(defmethod print-object ((universe universe) stream)
  (let ((u-name (list "static" "present" "future")))
    (dolist (universe-side (list (u-array-static universe) (u-array universe)
                                 (u-array-future universe)))
      (format stream "*** Side: ~a~%" (car u-name))
      (dotimes (x (size universe))
        (format stream "~2a|" x)
        (dotimes (y (size universe))
          (let ((elt-list (aref universe-side x y)))
            (print-elements elt-list stream)))
        (format stream "|")
        (terpri stream))
      (setf u-name (cdr u-name)))))


(defmethod place-static-elt-at ((universe universe) element x y)
  (if (typep element 'static-element)
      (setf (aref (u-array-static universe) x y) element)
      (error "The element is not a static element")))
 

(defmethod get-elt-at ((universe universe) element-type x y &key future)
  (when (or (minusp x) (minusp y) (>= x (size universe)) (>= y (size universe)))
    (return-from get-elt-at nil))
  (let ((array (if future (u-array-future universe) (u-array universe))))
    (dolist (elt (aref array x y))
      (when (eql (type-of elt) element-type)
        (return-from get-elt-at elt)))
    nil))


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

   (history
    :documentation "Used to avoid following its own tracks"
    :initform nil
    :accessor history)

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

   (ant-pheromone-fn
    :initarg :ant-pheromone-fn
    :initform nil
    :reader ant-pheromone-fn)

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

   (intensity
    :initarg :intensity
    :initform 10
    :accessor intensity)

   (owner
    :initarg :owner
    :initform nil
    :accessor owner)

   (decrease-rate
    :documentation "Decrease rate at every step (a fn.)"
    :initarg :decrease-rate
    :initform (lambda (x) (1- x))
    :reader decrease-rate))

  (:documentation "Pheromone class"))


    
(defgeneric entity-change-dir (entity degrees &key absolute)
  (:documentation "Change the entity direction with 'degrees' degrees or set the
  specified direction if 'absolute' is true"))

(defgeneric entity-try-move (ant universe x-step y-step)
  (:documentation "Try to move the specified entity in the universe. Return nil
  if not possible."))

(defgeneric entity-try-move-at (ant universe x-new y-new)
  (:documentation "Try to move the specified entity in the universe. Return nil
  if not possible."))


;; initialize x-step & y-step 
;; fixme: - these values should be computed again if direction is modified
(defmethod initialize-instance :after ((ant ant) &key)
  (setf (slot-value ant 'x-step) (* (step-size ant) (cos (direction ant))))
  (setf (slot-value ant 'y-step) (* (step-size ant) (sin (direction ant))))
  (setf (history ant) (list (cons (x ant) (y ant)))))

(defmethod print-object ((ant ant) stream)
  (format stream "ANT(~a)[~a,~a]" (or (id ant) "noname") (x ant) (y ant)))


(defmethod initialize-instance :after ((phe pheromone) &key)
  (when (null (decrease-rate phe))
    (setf (slot-value phe 'decrease-rate) (lambda (x) (1- x)))))

(defmethod print-object ((phe pheromone) stream)
  (format stream "PHE[~a,~a,int=~a]" (x phe) (y phe) (intensity phe)))


(defmethod entity-change-dir ((ant ant) rad &key absolute)
  (if absolute
      (setf (direction ant) rad)
      (setf (direction ant) (+ (direction ant) rad)))
  (setf (slot-value ant 'x-step) (* (step-size ant) (cos (direction ant))))
  (setf (slot-value ant 'y-step) (* (step-size ant) (sin (direction ant))))
  ;;(ant-log 6 "step-size for ant is " (step-size ant))
  ;;(ant-log 6 "cos & sin for ant is " (cos (direction ant)) " , "
  ;;         (sin (direction ant)))
  (ant-log 4 "new direction for ant is " (radtod (direction ant)))
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


(defmethod entity-try-move-at ((ant ant) (universe universe) x-new y-new)
  (let ((x-new-int (round x-new))
         (y-new-int (round y-new)))
    (ant-log 4 "candidate positions for ant: (" x-new "," y-new ")")
    ;; check free space (in present & future)
    (if (and (empty-p universe x-new-int y-new-int t) ; free space in the future
             ;; present - check if position is empty or is the current position
             ;; of the ant
             (or (empty-p universe x-new-int y-new-int nil)
                 (and (eql x-new-int (round (x ant)))
                      (eql y-new-int (round (y ant))))))
        (progn
          (place-elt-at universe ant x-new-int y-new-int :future t)
          (setf (x ant) x-new)
          (setf (y ant) y-new)
          (ant-log 4 "new position for ant: (" (x ant) "," (y ant) ")")
          t)
        (progn
          (ant-log 4 "cannot move to: (" x-new "," y-new ")")
          nil))))

    
(defmethod entity-try-move ((ant ant) (universe universe) x-step y-step)
  (let* ((x-new (+ (x ant) x-step))
         (y-new (+ (y ant) y-step)))
    (ant-log 5 "current params: x=" (x ant) " y=" (y ant) " x-step=" x-step
             " y-step=" y-step)
    (entity-try-move-at ant universe x-new y-new)))


(defun ant-try-move-direction (ant universe direction)
  "Try to move forward/backward/left/right"
  (multiple-value-bind (x-new-step y-new-step angle)
      (cond
        ((eql direction 'forward)
         (values (x-step ant) (y-step ant) 0))
        ((eql direction 'backward)
         (values (- (x-step ant)) (- (y-step ant)) 180))
        ((eql direction 'left)
         (values (- (y-step ant)) (x-step ant) 90))
        ((eql direction 'right)
         (values (y-step ant) (- (x-step ant)) -90)))
    (if (entity-try-move ant universe x-new-step y-new-step)
        (entity-change-dir ant (dtorad angle))
        nil)))


(defun ant-try-move-angle (ant universe angle &key absolute)
  (ant-log 2 "  trying to change angle"
           (if absolute " to " " with ") (radtod angle) " degrees")
  (let ((new-angle (if absolute angle (+ (direction ant) angle))))
    (let ((x-step (* (step-size ant) (cos new-angle)))
          (y-step (* (step-size ant) (sin new-angle))))
      (and (entity-try-move ant universe x-step y-step)
           (entity-change-dir ant new-angle :absolute t)))))


(defun ant-try-move-towards (ant universe x-final y-final)
  "Try to move towards coord. (x-final, y-final)"
  (ant-try-move-angle ant universe
                      (atan (- y-final (y ant)) (- x-final (x ant)))
                      :absolute t))


(defun ant-move-deterministic (ant universe)
  (ant-log 3 "ant " ant " in action")
  (or (ant-try-move-direction ant universe 'forward)
      (ant-log 4 "cannot move forward")
      (ant-try-move-direction ant universe 'left)
      (ant-log 4 "cannot move left")
      (ant-try-move-direction ant universe 'right)
      (ant-log 4 "cannot move right")
      (ant-try-move-direction ant universe 'backward)
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


(defun check-phe-around (universe x y r &optional history)
  "Find pheromones around (x,y), skipping coordinates from history list (if
provided). Return a list of pheromones."
  (let ((ret-list nil))
    (dolist (elt (dyn-elt-present universe))
      (when (eql (type-of elt) 'pheromone)
        (ant-log 7 "checking phe " elt)
        (let ((dist (distance-manhattan x y (x elt) (y elt)))
              (cond2 (notany (lambda (p)
                               (and (<= (abs (- (x elt) (car p))) 1)
                                    (<= (abs (- (y elt) (cdr p))) 1)))
                             history)))
          (ant-log 7 "dist=" dist ", cond2=" cond2)
          (when (and (<= dist r) cond2)
            (if ret-list
                (nconc ret-list (list elt))
                (setf ret-list (list elt)))))))
    ret-list))


(defun ant-try-follow-phe (ant universe)
  "Try to follow pheromone trails"
  (let* ((x (round (x ant))) (y (round (y ant)))
        (phe-list (check-phe-around universe x y *ant-smell-radius* (history ant))))
    (ant-log 5 "found pheromones around (" x "," y "): " phe-list)
    ;; :todo: - take into account distance
    (sort phe-list (lambda (a b) (> (intensity a) (intensity b))))
    (ant-log 5 "after sorting by intensity: " phe-list)
    (dolist (phe phe-list)
      (when (ant-try-move-towards ant universe (x phe) (y phe))
        (ant-log 5 "moving towards " (x phe) "," (y phe))
        (return-from ant-try-follow-phe t)))
    nil))


(defun ant-move-follow-phe-random (ant universe)
  "Follow pheromone or move random if no pheromone around"
  (unless (ant-try-follow-phe ant universe)
    ;; no pheromone around - try random move
    (ant-move-random-or-det ant universe)))


(defun ant-move-follow-phe-det (ant universe)
  "Follow pheromone or move deterministic if no pheromone around"
  (unless (ant-try-follow-phe ant universe)
    ;; no pheromone around - try deterministic move
    (ant-move-deterministic ant universe)))
        

(defmethod passing-time ((universe universe) (ant ant))
  (ant-log 3 "ant " ant " in action")
  (if (ant-move-func universe)
      (progn
        (ant-log 5 "calling custom func " (ant-move-func universe)
                 " for moving ant")
        (funcall (ant-move-func universe) ant universe))
      (ant-move-random-or-det ant universe))
  (when (ant-pheromone-fn ant)
    (ant-log 5 "calling pheromone func")
    (funcall (ant-pheromone-fn ant) ant universe)))


(defmethod passing-time ((universe universe) (phe pheromone))
  (ant-log 6 "pheromone " phe " in action")
  (if (plusp (intensity phe))
      (progn
        (setf (intensity phe) (funcall (decrease-rate phe) (intensity phe)))
        (place-elt-at universe phe (x phe) (y phe) :future t :from-past t)
        (ant-log 6 "pheromone " phe " after passing time."))
      (progn
        (ant-log 5 "pheromone " phe " has vanished")
        ;;(delete-elt-at universe 'pheromone (x phe) (y phe) :future nil)
        )))
        

(defmethod place-elt-at ((universe universe) (elt standard-object) x y
                         &key (future t))
  (let ((array (if future (u-array-future universe) (u-array universe)))
        (list (if future (dyn-elt-future universe) (dyn-elt-present universe))))
    (ant-log 3 "placing a new element at " x "," y " in "
             (if future "future" "present"))
    (if (null (aref array x y))
        (setf (aref array x y) (list elt))
        (progn
          (nconc (aref array x y) (list elt))
          (ant-log 5 "concat. with previous elements")))
    (if list
      (nconc list (list elt))
      (if future
          (setf (dyn-elt-future universe) (list elt))
          (setf (dyn-elt-present universe) (list elt))))))


(defmethod place-elt-at ((universe universe) (ant ant) x y &key (future t))
  (ant-log 3 "placing a new ant")
  (ant-log 3 "direction of ant: " (radtod (direction ant)))
  (nconc (history ant) (list (cons x y)))
  (when (>= (length (history ant)) *ant-history-len*)
    (pop (history ant)))
  (ant-log 6 "history of " ant " is now " (history ant))
  (call-next-method))

  
(defmethod place-elt-at ((universe universe) (phe pheromone) x y
                         &key (future t) from-past)
  (when (or (minusp x) (minusp y) (>= x (size universe)) (>= y (size universe))
            (not (plusp (intensity phe))))
    (ant-log 7 "pheromone out of bounds: " phe)
    (return-from place-elt-at))
  (let ((array (if future (u-array-future universe) (u-array universe)))
        (list (if future (dyn-elt-future universe) (dyn-elt-present universe)))
        (old-phe (get-elt-at universe 'pheromone x y :future future)))
    (ant-log 6 "placing a new pheromone at " x "," y " in "
             (if future "future" "present"))
    (if old-phe
        (progn
          (ant-log 6 "a pheromone already exists at the coords.")
          ;; :todo: - different types of pheromones
          ;; :todo: - different decrease rates
          (setf (intensity old-phe)
                (min *pheromone-max-intensity*
                     (+ (intensity old-phe) (intensity phe))))
          ;; replace owner (if not coming from past)
          (when (not from-past)
            (setf (owner old-phe) (owner phe))))
        (progn
          (if (null (aref array x y))
              (setf (aref array x y) (list phe))
              (progn
                (nconc (aref array x y) (list phe))
                (ant-log 6 "concat. with previous elements")))
          (if list
              (nconc list (list phe))
              (if future
                  (setf (dyn-elt-future universe) (list phe))
                  (setf (dyn-elt-present universe) (list phe))))))))

  
(defmethod delete-elt-at ((universe universe) elt-type x y &key future)
  (when (or (minusp x) (minusp y))
    (return-from delete-elt-at))
  (let ((elt (get-elt-at universe elt-type x y :future future)))
    (ant-log 3 "Removing an element of type " elt-type " from coords. "
             x "," y " in " (if future "future" "present"))
    (if elt
        (if future
            (progn
              (setf (dyn-elt-future universe)
                    (remove elt (dyn-elt-future universe) :test #'eq))
              (setf (aref (u-array-future universe) x y)
                    (remove elt (aref (u-array-future universe) x y) :test #'eq)))
            (progn
              (setf (dyn-elt-present universe)
                    (remove elt (dyn-elt-present universe) :test #'eq))
              (setf (aref (u-array universe) x y)
                    (remove elt (aref (u-array universe) x y) :test #'eq))))
        (ant-log 3 "Nothing to remove at coords. " x "," y))))
      

(defun place-phe-around (universe x y r &key decrease-rate (future t) owner)
  "Place pheromones on a square area around x,y with side r"
  ;; :fixme: r > 10 ?
  ;; :fixme: outside universe
  (flet ((make-phe (x y i)
           (make-instance 'pheromone :x x :y y :intensity i :owner owner
                          :decrease-rate decrease-rate)))
    (place-elt-at universe (make-phe x y (/ *pheromone-max-intensity* 2))
                  x y :future future )
    (let ((maxi (/ *pheromone-max-intensity* 2)))
      (loop
         for times from 1 to r do
           (ant-log 7 "horizontal from " (- x times) " to " (+ x times)
                    " at  y=" (- y times) "&" (+ y times))
           (loop
              for p-phe from (- x times) to (+ x times) do
                (place-elt-at universe (make-phe p-phe (- y times) (- maxi times))
                              p-phe (- y times) :future future)
              (place-elt-at universe (make-phe p-phe (+ y times) (- maxi times))
                            p-phe (+ y times) :future future))
         (ant-log 7 "vertical from " (1+ (- y times)) " to " (1- (+ y times))
                  " at  x=" (- x times) "&" (+ x times))
         (loop
            for p-phe from (1+ (- y times)) to (1- (+ y times)) do
              (place-elt-at universe (make-phe (- x times) p-phe (- maxi times))
                            (- x times) p-phe :future future)
              (place-elt-at universe (make-phe (+ x times) p-phe (- maxi times))
                            (+ x times) p-phe :future future))))))


(defun ant-phe-fn-1 (ant universe)
  "Ant leaving pheromone trails"
  (let ((x (round (x ant)))
        (y (round (y ant))))
    (place-elt-at universe
                  (make-instance 'pheromone :x x :y y
                                 :intensity *pheromone-max-intensity*
                                 :decrease-rate (lambda (x) (- x 0.33)))
                  x y :future t)))

(defun ant-phe-fn-2 (ant universe)
  "Ant leaving pheromone trails (r=3)"
  (let ((x (round (x ant)))
        (y (round (y ant))))
    (place-phe-around universe x y 2 :decrease-rate (lambda (x) (- x 0.1))
                      :owner ant)))


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
