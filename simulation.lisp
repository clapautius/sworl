(in-package :sworl)

;; :debug:
(declaim (optimize debug))

;;;; A universe = collection of objects and rules.
(defclass universe ()
  (
   ;; a list with objects in the universe
   (objects
    :initform nil
    :accessor objects)

   (rules-for-individuals
    :initform nil
    :accessor rules-one)

   ;; rules executed after rules-pair
   (post-rules-for-individuals
    :initform nil
    :accessor post-rules-one)

   (rules-for-pairs
    :initform nil
    :accessor rules-pair)

   ;; universal time
   (u-time
    :initform 0
    :accessor u-time)

   (txt-file-name
    :initform nil
    :initarg :file-name
    :accessor txt-file-name))

  (:documentation "Universe = collection of objects and rules."))


(defmethod tick ((universe universe))
  "Update all objects and advance time with 1 time unit (one second)."
  ;; apply rules for individual objects
  (let ((rules (rules-one universe)))
    (when rules
      (dolist (obj (objects universe))
        (dolist (rule rules)
          (funcall rule obj)))))

  ;; apply rules for pairs of objects
  (let ((rules (rules-pair universe)))
    (when rules
      (do ((pos1 (objects universe) (cdr pos1)))
          ((null pos1))
        (dolist (obj2 (cdr pos1))
          (dolist (rule rules)
            (funcall rule (car pos1) obj2))))))

  ;; apply post rules for individual objects
  (let ((rules (post-rules-one universe)))
    (when rules
      (dolist (obj (objects universe))
        (dolist (rule rules)
          (funcall rule obj)))))

  ;; update objects
  (dolist (obj (objects universe))
    (update obj))

  (incf (u-time universe))

  ;; :fixme: keep the file open
  (when (txt-file-name universe)
    (with-open-file (sim-file (txt-file-name universe) :direction :output :if-exists :append
                              :if-does-not-exist :create)
      (format sim-file "t=~a~%" (u-time universe))
      (format sim-file "n=~a~%" (length (objects universe)))
      (dolist (obj (objects universe))
        (format sim-file "x=~a~%y=~a~%z=~a~%vx=~a~%vy=~a~%vz=~a~%"
                (particle-loc-x obj)
                (particle-loc-y obj)
                (particle-loc-z obj)
                (particle-vect-x obj velocity)
                (particle-vect-y obj velocity)
                (particle-vect-z obj velocity)))))
  
  (format t "Universal time: ~a~%" (u-time universe))
  ;;(dolist (obj (objects universe))
  ;;  (format t "- ~a~%" obj))
  
  t)


;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
