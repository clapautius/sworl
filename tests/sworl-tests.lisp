(in-package :sworl)


(defun sworl-test-case (test-name expr1 expr2)
  (let ((ok t))
    (when (not (equalp expr1 expr2))
      (format t "Test ~a failed~%  result: ~a~%  correct result: ~a~%"
              test-name expr1 expr2)
      (setf ok nil))
    ok))

  
;;; vector tests

(defun sworl-run-tests ()
  "Run all SWORL tests."
  (let* ((location1 (make-array 3 :initial-contents '(0 0 0)))
         (location2 (make-array 3 :initial-contents '(0 10 0))))
    (sworl-test-case "vector-from-to #1"
                     (vector-from-to location1 location2 5)
                     (make-array 3 :initial-contents '(0.0 5.0 0.0)))
    (sworl-test-case "vector-from-to #2"
                     (vector-from-to location2 location1 1)
                     (make-array 3 :initial-contents '(0.0 -1.0 0.0)))))
