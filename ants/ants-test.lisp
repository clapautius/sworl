(in-package :sworl.ants)


(defmacro test-case (test &optional (string "Testing "))
  `(progn
	 (format t "~a ~a ..." ,string ',test)
	 (if ,test
		 (progn
		   (format t " OK~%")
		   t)
		 (progn
		   (format t " FAILED~%")
		   nil))))
  
(defun ants-test-all ()
  (let ((test-universe (make-instance 'universe :size 5 :max-age 10))
		(ant1 (make-instance 'ant :x 2 :y 2 :x-dir 1 :y-dir 1 :step-size 1.4))
		(ant2 (make-instance 'ant :x 1 :y 2 :x-dir 1 :y-dir 1 :step-size 1.4)))
	(setf (aref (u-array test-universe) 2 2) ant1)
	(setf (aref (u-array test-universe) 1 2) ant2)

	;; advance one sec.
	(passing-time-universal test-universe)
	(ant-try-move-left ant2 test-universe)
	(test-case (empty test-universe 2 2 nil))
	(test-case (not (empty test-universe 3 3 nil)))
	(test-case (and (= (round (x ant2)) 1) (= (round (y ant2)) 4)))
	(test-case (not (empty test-universe 1 4 t))) ; check in the future
))
