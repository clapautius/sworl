(in-package :sworl.ants)

(defmacro test-case (test &optional (string "Testing "))
  `(progn
     (format t "  ~a ~a ..." ,string ',test)
     (if ,test
         (progn
           (format t " OK~%")
           t)
         (progn
           (format t " FAILED~%")
           (setf error-flag t)
           nil))))

  
(defun ants-test-all ()
  (let ((test-universe (make-instance 'universe :size 5 :max-age 10))
		(ant1 (make-instance 'ant :x 2 :y 2 :direction (dtorad 45) :step-size 1.4))
		(ant2 (make-instance 'ant :x 1 :y 2 :direction (dtorad 45) :step-size 1.4))
        (error-flag nil))
	(place-element-at test-universe ant1 2 2 :future nil)
	(place-element-at test-universe ant2 1 2 :future nil)

	;; advance one sec.
	(passing-time-universal test-universe)
	(ant-try-move-left ant2 test-universe)
    (test-case (not (empty test-universe 5 5 nil)))
	(test-case (empty test-universe 2 2 nil))
	(test-case (not (empty test-universe 3 3 nil)))
	(test-case (and (= (round (x ant2)) 1) (= (round (y ant2)) 4)))
	(test-case (not (empty test-universe 1 4 t))) ; check in the future

    (if error-flag
        (format t "At least one error encountered!")
        (format t "Everything is OK"))))


(defun test-generate-small-universe ()
  "Generate a small universe with everything (for testing)"
  (let ((small-universe (make-instance 'universe :size 8 :max-age 50)))
    (place-static-element-at small-universe 'rock 5 5)
    small-universe))


(defun ants-run-opengl1 (&key (size 100) (duration 150))
  "Simulation 1 (opengl) : 2 ants"
  (ant-log 0 "Run an opengl simulation in an universe of size " size)
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((test-universe (make-instance 'universe :size size :max-age duration))
		(ant1 (make-instance 'ant :x 0 :y 50 :direction (dtorad 45) :step-size 1.4))
		(ant2 (make-instance 'ant :x 20 :y 0 :direction (dtorad 45) :step-size 1.4 :color 'green)))
	(place-element-at test-universe ant1 0 50 :future nil)
	(place-element-at test-universe ant2 0 0 :future nil)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :pause 0.25 :universe test-universe)))
  (ant-log 0 "That's all folks!"))


(defun ants-run-opengl2 (&key (size 100) (duration 15))
  "Simulation 2 (opengl) : one ant turning left at the margin"
  (ant-log 0 "Run an opengl simulation in an universe of size " size)
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((test-universe (make-instance 'universe :size size :max-age duration))
		(ant1 (make-instance 'ant :x 90 :y 0 :direction (dtorad 45) :step-size 1.4)))
	(place-element-at test-universe ant1 0 0 :future nil)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :pause 0.25 :universe test-universe)))
  (ant-log 0 "That's all folks!"))



(defun ants-run-opengl-face-to-face (&key (duration 150))
  "Run the simulation"
  (ant-log 0 "Run a simulation with 2 ants in an universe of size 200")
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((universe (make-instance 'universe :size 201 :max-age duration))
        (ant1 (make-instance 'ant :x 0 :y 100 :direction 0))
        (ant2 (make-instance 'ant :x 200 :y 100 :direction (dtorad 180) :color 'blue)))
    (place-element-at universe ant1 0 100 :future nil)
    (place-element-at universe ant2 200 100 :future nil)
    (glut:display-window (make-instance 'u-window :width 201 :height 201
                                        :pause 0.05 :universe universe)))
  (ant-log 0 "That's all folks!"))



;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
