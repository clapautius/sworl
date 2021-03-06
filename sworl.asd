;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'sworl) or
;;;   (require 'sworl)

(defpackage #:sworl-system (:use #:cl #:asdf))
(in-package :sworl-system)

(require 'cl-glut)
(require 'cl-glu)
(require 'cl-opengl)
(require 'zpng)
(require 'log4cl)

(asdf:defsystem :sworl
    :components ((:file "package")
                 (:file "vector"
                        :depends-on ("package"))
                 (:file "particle"
                        :depends-on ("package"))
                 (:file "newton"
                        :depends-on ("package"))
                 (:file "simulation"
                        :depends-on ("package"))
                 (:file "simulation-visual"
                        :depends-on ("package"))
                 (:file "simulation-test"
                        :depends-on ("package"))
                 (:file "starling-sim"
                        :depends-on ("package"))
                 (:file "utils"
                        :depends-on ("package"))
                 (:file "tests/sworl-tests"
                        :depends-on ("package"))
                 (:file "ants/ants"
                        :depends-on ("package"))
				 (:file "ants/ants-visual"
						:depends-on ("package"))))

;;; * emacs display settings *
;;; Local Variables:
;;; default-tab-width: 4
;;; indent-tabs-mode: nil
;;; End:
