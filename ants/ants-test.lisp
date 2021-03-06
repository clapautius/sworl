(in-package :sworl.ants)

(declaim (optimize debug))

(defmacro test-case (test &optional (string "Testing: "))
  `(progn
     (format t "~a~%  ~a ..." ,string ',test)
     (if ,test
         (progn
           (format t " OK~%")
           t)
         (progn
           (format t " FAILED~%")
           (setf error-flag t)
           nil))))


(defun ants-make-test-universe ()
  "Generate universe used by 'ants-test-all'"
  (let ((test-universe (make-instance 'universe :size 5 :max-age 10
                                      :ant-move-func 'ant-move-deterministic))
		(ant1 (make-instance 'ant :x 2 :y 2 :direction (dtorad 45)
                             :step-size 1.4 :id "ant1"))
		(ant2 (make-instance 'ant :x 1 :y 2 :direction (dtorad 45)
                             :step-size 1.4 :id "ant2"))
        (phe1 (make-instance 'pheromone :x 2 :y 2 :intensity 5))
        (phe2 (make-instance 'pheromone :x 0 :y 0 :intensity 5))
        (phe3 (make-instance 'pheromone :x 0 :y 0 :intensity 7))
        (phe4 (make-instance 'pheromone :x 1 :y 1 :intensity 8
                             :decrease-rate (lambda (x) (/ x 2)))))
    (place-elt-at test-universe phe1 2 2 :future nil)
	(place-elt-at test-universe ant1 2 2 :future nil)
	(place-elt-at test-universe ant2 1 2 :future nil)
    (place-elt-at test-universe phe2 0 0 :future nil)
    (place-elt-at test-universe phe3 0 0 :future nil)
    (place-elt-at test-universe phe4 1 1 :future nil)
    (place-static-elt-at test-universe 'rock 1 0)
    test-universe))


;;; :todo: - add tests with pheromone owner, replacing owner, etc.
(defun ants-test-all ()
  (let* ((test-universe (ants-make-test-universe))
         (error-flag nil)
         (ant2 (get-elt-at test-universe 'ant 1 2))
         (phe4 (get-elt-at test-universe 'pheromone 1 1)))

    ;; check ant & pheromone
    (test-case (not (empty-p test-universe 2 2 nil)))
	;; advance one sec.
	(passing-time-universal test-universe)
    ;; check movement
	(ant-try-move-direction ant2 test-universe 'left)
    (test-case (not (empty-p test-universe 5 5 nil)))
    ;; check ants
	(test-case (empty-p test-universe 2 2 nil))
	(test-case (not (empty-p test-universe 3 3 nil)))
	(test-case (and (= (round (x ant2)) 1) (= (round (y ant2)) 4)))
    ;; check pheromones
    (test-case (not (null phe4)))
    (test-case (empty-p test-universe 0 0 nil))
    (test-case (empty-p test-universe 1 1 nil))
    (test-case (and phe4 (eq (get-elt-at test-universe 'pheromone 1 1) phe4)))
    (test-case (and phe4 (eql (intensity phe4) 4)))
    ;; delete a pheromone
    (delete-elt-at test-universe 'pheromone 1 1 :future nil)
    (test-case (null (get-elt-at test-universe 'pheromone 1 1 :future nil))
               "Testing empty cell after removing pheromone: ")

    (test-case (not (empty-p test-universe 1 0 nil))
               "Testing static elt: ")

    ;; check in the future
	(test-case (not (empty-p test-universe 1 4 t)))

    (if error-flag
        (format t "At least one error encountered!")
        (format t "Everything is OK"))))


(defun test-generate-small-universe ()
  "Generate a small universe with everything (for testing)"
  (let ((small-universe (make-instance 'universe :size 8 :max-age 50)))
    (place-static-elt-at small-universe 'rock 5 5)
    small-universe))


;;; simulation helper functions

;;; testing functions
(defun generate-ants (number universe &optional (random-direction nil))
  "Generate <number> ants and place them in the universe"
  (flet ((get-free-space (universe)
           "Find a free space in the universe (return multiple values)"
           (dotimes (x (size universe))
             (dotimes (y (size universe))
               (if (empty-p universe x y nil)
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
        (place-elt-at universe
                      (if random-direction
                          (make-instance 'ant :x x-empty :y y-empty
                                         :direction (dtorad (random 360)))
                          (make-instance 'ant :x x-empty :y y-empty))
                      x-empty y-empty :future nil)
        (ant-log 3 "placing a new ant at " x-empty "," y-empty)
        (ant-log 3 "direction: " (direction (aref (u-array universe)
                                                  x-empty y-empty)))))))


;;; testing functions
(defun generate-ants-random (number universe &optional (random-direction nil))
  "Generate <number> ants and place them in the universe"
  (dotimes (i number)
    (loop 
       (let ((x-proposed (random (size universe)))
             (y-proposed (random (size universe)))
             (color (nth (random 6) (list 'red 'green 'blue 'yellow 'cyan 'magenta)))
             (name (format nil "ant~a" i)))
         (when (empty-p universe x-proposed y-proposed nil)
           (let ((ant (if random-direction
                          (make-instance 'ant :x x-proposed :y y-proposed
                                         :id name :color color
                                         :ant-pheromone-fn 'ant-phe-fn-1
                                         :direction (dtorad (random 360)))
                          (make-instance 'ant :x x-proposed :y y-proposed
                                         :id name :color color
                                         :ant-pheromone-fn 'ant-phe-fn-1))))
             (place-elt-at universe ant x-proposed y-proposed :future nil)
             (ant-log 3 "New ant generated: " ant "; color=" (color ant)))
           (return t))))))


(defun ants-pre-run (universe ants-no &optional (opengl t))
  (ant-log 0 "Run " (if opengl "opengl" "non-graphic")
           " simulation in an universe of size " (size universe))
  (ant-log 0 "Duration of the simulation: " (u-max-age universe)
           ". No. of ants: " ants-no))


(defun ants-post-run ()
  (ant-log 0 "That's all folks!"))


(defun ants-run-opengl1 (&key (size 100) (duration 150))
  "Simulation 1 (opengl) : 2 ants"
  (let ((test-universe (make-instance 'universe :size size :max-age duration))
		(ant1 (make-instance 'ant :x 0 :y 50 :direction (dtorad 45) :step-size 1.4))
		(ant2 (make-instance 'ant :x 20 :y 0 :direction (dtorad 45) :step-size
                             1.4 :color 'green)))
    (ants-pre-run test-universe 2)
	(place-elt-at test-universe ant1 0 50 :future nil)
	(place-elt-at test-universe ant2 0 0 :future nil)
    (place-static-elt-at test-universe 'rock 30 10)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :pause 0.25 :universe test-universe)))
  (ants-post-run))


(defun ants-run-opengl2 (&key (size 100) (duration 15))
  "Simulation 2 (opengl) : one ant turning left at the margin"
  (let ((test-universe (make-instance 'universe :size size :max-age duration))
		(ant1 (make-instance 'ant :x 90 :y 0 :direction (dtorad 45) :step-size
                             1.4)))
    (ants-pre-run test-universe 1)
	(place-elt-at test-universe ant1 0 0 :future nil)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :pause 0.25 :universe test-universe)))
  (ants-post-run))


(defun ants-run-opengl3 (&key (size 100) (duration 250))
  "Simulation 3 (opengl) : ants moving randomly"
  (ant-log 0 "Run an opengl simulation in an universe of size " size)
  (ant-log 0 "Duration of the simulation: " duration)
  (let ((test-universe (make-instance 'universe :size size :max-age duration))
		(ant1 (make-instance 'ant :x 50 :y 50 :direction (dtorad 45)
                             :step-size 1.4))
        (ant2 (make-instance 'ant :x 30 :y 30 :direction (dtorad 45)
                             :step-size 1.4 :color 'green)))
    (ants-pre-run test-universe 2)
	(place-elt-at test-universe ant1 50 50 :future nil)
	(place-elt-at test-universe ant2 30 30 :future nil)
    (place-static-elt-at test-universe 'rock 30 10)
    (dotimes (i 35)
      (place-static-elt-at test-universe 'rock (+ i 60) 60))
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :pause 0.1 :universe test-universe)))
  (ants-post-run))


(defun ants-run-opengl-many-random (&key (size 500) (ants 20) (duration 1500)
                                    function pause)
  "Simulation with many ants with random positions and random directions"
  (let* ((*random-state* (make-random-state t))
         (fn (if function function 'ant-move-follow-phe-random))
         (universe (make-instance 'universe :size size :max-age duration
                                  :ant-move-func fn)))
    (ants-pre-run universe ants)
    (generate-ants-random ants universe t)
    (glut:display-window (make-instance 'u-window :width size :height size
                                        :universe universe :keep-trails nil
                                        :pause pause)))
  (ants-post-run))


(defun ants-run-opengl-one-random (&key (size 200) (duration 250)
                                   function (pause 0.15))
  "Simulation with one ant with random position & random directions"
  (ants-run-opengl-many-random :ants 1 :size size :duration duration
                               :function function :pause pause))


(defun ants-run-opengl-face-to-face (&key (duration 150))
  "Run the simulation (two ants walking towards each other)"
  (let ((universe (make-instance 'universe :size 150 :max-age duration
                                 :ant-move-func 'ant-move-follow-phe-det))
        (ant1 (make-instance 'ant :x 75 :y 100 :direction 0
                             :id "left2right" :ant-pheromone-fn 'ant-phe-fn-1
                             :color 'red))
        (ant2 (make-instance 'ant :x 125 :y 100 :direction (dtorad 180)
                             :id "right2left" :ant-pheromone-fn 'ant-phe-fn-1
                             :color 'green)))
    (ants-pre-run universe 2)
    (place-elt-at universe ant1 75 100 :future nil)
    (place-elt-at universe ant2 125 100 :future nil)
    ;;(break)
    (glut:display-window (make-instance 'u-window :width (size universe)
                                        :height (size universe)
                                        :pause 0.05 :universe universe)))
  (ants-post-run))


(defun ants-run-opengl-cross (&key (duration 150))
  "Run the simulation (two ants intersecting)"
  (let ((universe (make-instance 'universe :size 150 :max-age duration
                                 :ant-move-func 'ant-move-follow-phe-det))
        (ant1 (make-instance 'ant :x 83 :y 100 :direction 0
                             :id "left2right" :ant-pheromone-fn 'ant-phe-fn-1
                             :color 'red))
        (ant2 (make-instance 'ant :x 100 :y 110 :direction (dtorad 270)
                             :id "down" :ant-pheromone-fn 'ant-phe-fn-1
                             :color 'green)))
    (ants-pre-run universe 2)
    (place-elt-at universe ant1 83 100 :future nil)
    (place-elt-at universe ant2 100 110 :future nil)
    ;;(break)
    (glut:display-window (make-instance 'u-window :width (size universe)
                                        :height (size universe)
                                        :pause 0.05 :universe universe)))
  (ants-post-run))


(defun ants-run (&key (size 500) (ants 10) (duration 100))
  "Run the simulation"
  (let ((universe (make-instance 'universe :size size)))
    (ants-pre-run universe ants nil)
    (generate-ants ants universe t)
    (dotimes (i duration)
      (passing-time-universal universe)))
  (ants-post-run))


(defun ants-run-opengl-3d-many-random (&key (size 500) (ants 20)
                                         (duration 1500) function pause)
  "Simulation with many ants with random positions and random directions"
  (let* ((*random-state* (make-random-state t))
         (fn (if function function 'ant-move-follow-phe-random))
         (universe (make-instance 'universe :size size :max-age duration
                                  :ant-move-func fn)))
    (ants-pre-run universe ants)
    (generate-ants-random ants universe t)
    (glut:display-window (make-instance 'u-3d-window :width size :height size
                                        :universe universe :keep-trails nil
                                        :pause pause)))
  (ants-post-run))


(defun ants-run-opengl-3d-one-random (&key (size 200) (duration 250)
                                        function (pause 0.15))
  "Simulation with one ant with random position & random directions"
  (ants-run-opengl-3d-many-random :ants 1 :size size :duration duration
                               :function function :pause pause))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
