;;; run from SLIME or make a shortcut for: 
;;;   (asdf:oos 'asdf:load-op 'sworl) or
;;;   (require 'sworl)

(defpackage #:sworl-system (:use #:cl #:asdf))
(in-package :sworl-system)

(require 'cl-glut)
(require 'cl-glu)
(require 'cl-opengl)

(asdf:defsystem :sworl
    :components ((:file "package")
                 (:file "particle"
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
