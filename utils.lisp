;; various useful functions used in simulations but not strictly related to simulations

(in-package :sworl)


(defun random-limits (min-limit max-limit)
  "Return an integer between MIN-LIMIT and MAX-LIMIT (less than MAX-LIMIT)."
  (+ (random (- max-limit min-limit)) min-limit))
