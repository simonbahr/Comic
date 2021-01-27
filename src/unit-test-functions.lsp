;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                              ;;;
;;;                             COMIC                            ;;;
;;;                                                              ;;;
;;;       Character-Oriented Media-Independent Composition       ;;;
;;;                     A Common Lisp Library                    ;;;
;;;                         by Simon Bahr                        ;;;
;;;                                                              ;;;
;;;                      UNIT TEST FUNCTIONS                     ;;;
;;;                                                              ;;;
;;;                                                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

(defun test-func (expected-result function &rest arguments)
  "a unit test function for checking functions"
  (let ((result (apply function arguments)))
    (if (equal expected-result result)
	t
	(error "--------COMIC-------u-%~
                FAILED UNIT TEST in DEFINITION OF ~
                ~s~%Arguments: ~s~%Result: ~s~%~
                Expected result: ~s.~%File: ~s~%"
	       function arguments result
	       expected-result *load-truename*))))
;;testing test functions with build-in procedures
(test-func 5 '+ 1 4)

(defun test-type (object expected-type)
  "a unit test function for checking types"
  (let ((type (type-of object)))
    (if (equal type expected-type)
	t
	(error "--------COMIC-------u-%~
FAILED UNIT TEST WHEN CHECKING TYPE~%Object: ~s~%~
Type: ~s~%Expected Type: ~s~%File: ~s~%"
	       object type expected-type *load-truename*))))

;;testing test functions with build-in procedures
(test-type 1.0 'single-float)
(test-type "Testing test-comic-type" '(SIMPLE-ARRAY CHARACTER (23)))

(defun test-var (quoted-variable &optional expected-value)
  "a unit test function for variables. ~
if value is not set, it will only check if the symbol is bound."
  (if (null expected-value)
      (if (boundp quoted-variable)
	  t
	  (error "--------COMIC-------u-%~
FAILED UNIT TEST WHEN CHECKING VARIABLE~%Symbol: ~s~%~
Error: The symbol is not bound~%" quoted-variable))
      (let ((value (eval quoted-variable)))
	(if (equal value expected-value)
	    t
	    (error "--------COMIC-------u-%~
FAILED UNIT TEST WHEN CHECKING VARIABLE~%Symbol: ~s~%~
Value: ~s~%Expected value: ~s~%" quoted-variable value expected-value)))))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EOF comic/unit-test-functions.lisp
