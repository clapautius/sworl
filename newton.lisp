;;;; various functions related to newtonian simulations (gravity & co.)

(defun orbital-speed (mass-big mass-small distance)
  (sqrt (/ (* mass-big mass-big 6.67384e-11) (* (+ mass-big mass-small) distance))))



;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
