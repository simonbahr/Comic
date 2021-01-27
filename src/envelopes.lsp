;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    *=*:        ;;
;;                                         -   -  -  -              :===:         ;;
;;                                                                 *==:           ;;
;;        :-          -  --  +  +- :+  *  +- :+  =  *: :+  *  ::-=*-    :++:      ;;
;;         *===:     +===*    -       -+++-     :=:           - +*   *WWWWWWWWW@- ;;
;;         =======*-*  +===* -=+-+****++++++++++++*#@@W#*==#WWW+   -WWWW+   -#WWW:;;
;;            -+*####=+   :===*+++++++*#@@WW*+++++WWWWW@+++=WWW=: -@WWW  -    :-  ;;
;;               -  :+*####WWW@#*++++++WWWWWW++++*WWWWWW*++*WWW@++=WWW*           ;;
;;       -:::-  -:  :-=WWWWWWWWWWW@++++@WWWWW@+++*WW#WWW#+++#WWW++*WWW@- --       ;;
;;   -#WWWWWWWWW+:+*=@WWW=+++++*WWWW*++=WWW=WW#++=WW*@WW@+++*WWW=++#WWW=*:     #WW;;
;;  =WWW#-:  =WWW#=*=WWW*++++++++@WW@++*WWW#=WW=+=WW*#WWW*+++WWW#+++WWWW:  :+-*WWW;;
;; +WWW:      -:- *=#WWW+++++++++=WWW=++@WW@+#WW*#WW+*WWW#+++#WWW+++*@WWWWWWWWWW@-;;
;; #WW@   -  -: -#=+=WWW*+*+*++++=WWW=++#WWW*+@WW@W@++@WW@+++=WWW=+*++**#WWW@=:   ;;
;; =WWW      -+*#=***WWWW**+*+**+@WWW+*+*WWW#++@WWW@++#WWW*+++#=**+++++=: -       ;;
;; :WWW*       +WWW=**WWWW#****#WWWW*****@WW@***@#=*+*+*++++++++++++++**          ;;
;;  *WWW* -  -*WWW@#***=WWWWWWWWWW=*****************************+*+**+*=*-        ;;
;;   :WWWWWWWWWWW@@@#******===*************************************=#---          ;;
;;     :@WWWWWW=:: :++#=******************************************=*              ;;
;;  -@@@@@#*-  -=#: --+==***********************************=*##=*=#:             ;;
;;   @=:   -*@@*       =====##*=#=**=**********************==+= +@=-              ;;
;;                   -  :  +*@@#*:**==***=****=*****==+--:*#- -@* +@@@+           ;;  
;;                      :#@@#- :+      +#:*+-   -*#-            #@- :#==*:        ;;
;;                   *@@@@#-             -                                        ;;
;;                      :-                                                        ;;
;; COMIC 1                                                                        ;;
;; Media-Integrative Composition in Common Lisp                        Simon Bahr ;;      
;; utilities/envelopes.lsp                                                   2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;; Probably add the envelopes to CLOS in some way? Make a class to allow for different
;;; coding styles.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/env-maker
;;; Name
;;; env-maker
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Returns a complex envelope as list. Takes a procedure to create
;;; e.g. logarithmic or exponential envelope segments.
;;;
;;; Parameter
;;; proc: a mathemathical function that is applied on each point
;;;       of the linear segments.
;;; start: first point of the line
;;; points-and-steps: a list of lists and numbers indicating the
;;;                   points along the env
;;;                   and the steps between them
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (env-maker (lambda (x) x) 0 '((1 2) (0 3))) --> (0 1/2 1 2/3 1/3 0)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun env-maker (proc start points-and-steps)
  "Makes an envelope with linear segments"
  ;; make a helper function make-line that returns a single
  ;; envelope segment
  (labels ((make-line (proc min max steps)
	     (let* ((reverse? (if (> min max) t))
		    (ls (loop for i from 0 to steps collect
			     (funcall proc i)))
		    (last (car (last ls))))
	       (if reverse? (setf min (prog1 max (setf max min))))
	       (setf ls (mapcar (lambda (x) (+ min (* x (/ (- max min) last)))) ls))
	       (if reverse?
		   (reverse ls)
		   ls))))
    ;; apply make-line for each segment and return the list
    (unless (null points-and-steps)
      (let ((end (if (listp (car points-and-steps))
		     (caar points-and-steps)
		     (car points-and-steps)))
	    (steps (if (listp (car points-and-steps))
		       (cadar points-and-steps) 1)))
	(append (make-line proc start end steps)
		(cdr (env-maker proc end (cdr points-and-steps))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/line
;;; Name
;;; line
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes a linear envelope.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (line 0 '(1 3)) --> (0 1/3 2/3 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun line (start &rest points-and-steps)
  "Makes a linear envelope from a given start point to other '(points in-n-steps)"
  (env-maker (lambda (x) x) start points-and-steps))
	       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/expo-n
;;; Name
;;; expo-n
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an exponential envelope.
;;; 
;;; Arguments
;;; power: the power of the exponential function (only positive,
;;;        negative sign is ignored)
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (expo-n 2 0 '(1 3)) --> (0 1/9 4/9 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expo-n (power start &rest points-and-steps)
  (env-maker
   (lambda (x) (expt x (abs power))) start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/expo
;;; Name
;;; expo
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an exponential envelope with a power of 2.
;;; Can also be called as 'expo2'.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (expo 0 '(1 3)) --> (0 1/9 4/9 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expo (start &rest points-and-steps)
  (env-maker (lambda (x) (expt x 2)) start points-and-steps))

(defun expo2 (start &rest points-and-steps)
  (env-maker (lambda (x) (expt x 2)) start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/expo3
;;; Name
;;; expo3
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an exponential envelope with a power of 3.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (expo3 0 '(1 3)) --> (0 1/27 8/27 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expo3 (start &rest points-and-steps)
  (env-maker (lambda (x) (expt x 3)) start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/expo4
;;; Name
;;; expo4
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an exponential envelope with a power of 4.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (expo4 0 '(1 3)) --> (0 1/81 16/81 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expo4 (start &rest points-and-steps)
  (env-maker (lambda (x) (expt x 4)) start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/expo5
;;; Name
;;; expo5
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an exponential envelope with a power of 5.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (expo3 0 '(1 3)) --> (0 1/243 32/243 1)
;;;
;;; Last Modified
;;; 2019/07/27
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun expo5 (start &rest points-and-steps)
  (env-maker (lambda (x) (expt x 5)) start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/loga-n
;;; Name
;;; loga-n
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an logarithmic envelope. Ignores negative sign in 
;;; base-value, as this would produce complex numbers.
;;; 
;;; Arguments
;;; base: the logarithm base
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (loga-n 10 1 '(10 3)) --> (1.0 6.6783676 10.0)
;;;
;;; Bugs
;;; - Always returns floats for some reason...
;;; - Returns the same result with different bases. Might be because
;;; of the make-line implementation. As a result, this function does
;;; not give more flexibility than 'loga'/'loga10' for now.
;;; - For some values the start-value is not returned when ramping
;;; down, instead, the end-value is appended to the list twice...
;;;
;;; Last Modified
;;; 2019/07/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loga-n (base start &rest points-and-steps)
  (cdr (env-maker (lambda (x) (if (equal x 0) 0 (log x (abs base))))
		 start points-and-steps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/loga
;;; Name
;;; loga
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an logarithmic envelope with a base of 10.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (loga 0 '(1 5)) --> (0.0 0.43067658 0.6826062 0.86135316 1.0)
;;;
;;; Bugs
;;; see 'loga-n'
;;;
;;; Last Modified
;;; 2019/07/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun loga-10 (start &rest points-and-steps)
  (cdr (env-maker
	(lambda (x) (if (equal x 0) 0 (log x 10)))
	start points-and-steps)))

(defun loga (start &rest points-and-steps)
  (apply 'loga-10 start points-and-steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/fibo
;;; Name
;;; fibo
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Makes an envelope based on the fibonacci series.
;;; 
;;; Arguments
;;; start: starting point
;;; points-and-steps: touples of points and steps to go there
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (fibo 0 '(5 5)) --> (0 1 1 2 3 5)
;;; (fibo 0 '(1 5)) --> (0 1/5 1/5 2/5 3/5 1)
;;;
;;; Last Modified
;;; 2020/01/15
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fibo (start &rest points-and-steps)
  "Makes an envelope from a given start point to other
  '(points in-n-steps) based on the fibaonacci series: Each segment 
  is generated in its usual form first, and then scaled to the needed
  range."
  (labels (;; make a fibonacci series with n elements
	   (fib (n zero one)
	     (if (eq 0 n)
		 nil
		 (cons one
		       (fib (1- n) one (+ zero one)))))
	   ;; make and scale a fibonacci series to any range,
	   ;; preserving the relations between values
	   (fib-from-to (n from to)
	     (let* ((ls (fib n 0 1))
		    (last (car (last ls))))
	       (loop for elem in ls collect
		    (+ from (* (- to from) (/ elem last)))))))
    ;; generate env segments
    (cons start
	  (loop for (tar steps) in points-and-steps
	     append
	       (prog1
		   (fib-from-to (floor steps) start tar)
		 (setq start tar))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/linpol
;;; Name
;;; linpol
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; Interpolates an envelope to a desired length.
;;; 
;;; Arguments
;;; target-length: a positive integer
;;; envelope
;;; float (:key, default: nil): if t, all returned values are floats
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (linpol 5 '(1 2 3 4) :float t) --> (1 1.75 2.5 3.25 4.0)
;;;
;;; Last Modified
;;; 2019/07/31
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linpol (target-length envelope &key (float nil))
  "stretches an envelope by a factor, using linear interpolation"
  (if (or (>= 0 target-length) (not (integerp target-length)))
      (error "linpol: target-value be a positive integer"))
  (unless (null envelope)
    (cond ((= 1 target-length)
	   (list (car envelope)))
	  ((null (cdr envelope))
	   (set-length target-length envelope))
	  (t (let* ((tar (1- target-length))
		    (env envelope)
		    (blasted-ls (cons (car env)
				      (loop for elem in env
					 for next-elem in (cdr env) append
					   (cdr
					    (line (if float (float elem) elem)
						  (list next-elem (expt tar 2))))))))
	       (loop for elem in blasted-ls
		  by (lambda (ls) (nthcdr (/ (1- (length blasted-ls)) tar) ls))
		  collect elem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/mix-envs
;;; Name
;;; mix-envs
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; mixes envelopes. the resulting envelope length is the sum of
;;; all partial envelopes. all values remain.
;;; 
;;; Arguments
;;; envelopes (rest)
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (mix '(1 2 3) '(4 5) '( 6 7 8 9)) -> (1 4 6 7 2 5 8 3 9)
;;;
;;; Last Modified
;;; 2019/10/04
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mix-envs (&rest envelopes)
  "mixes envelopes. resulting envelope length is the sum of all
   envelopes, all values remain" 
  (apply 'merge-ls 
	 (loop for env in envelopes collect
	      (nil-stretch (max-length envelopes) env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/mix-linpol
;;; Name
;;; mix-linpol
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; mixes envelopes using linear interpolation. the resulting 
;;; envelope length is the length of the longest partial envelope
;;; times the amount of partial envelopes. values may differ.
;;; 
;;; Arguments
;;; envelopes (rest)
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (mix-linpol '(1 2 3) '(4 5) '( 6 7 8 9)) -> ???
;;;
;;; Last Modified
;;; 2019/10/05
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mix-linpol (&rest envelopes)
  "mixes envelopes using linear interpolation"
  (let ((min-len (apply 'max-length envelopes)))
    (apply 'merge-ls
	   (loop for env in envelopes collect
		(linpol min-len env :float t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/mix-loop
;;; Name
;;; mix-loop
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; mixes envelopes by looping the values of the shorter ones. 
;;; resulting length is the length of the longest partial envelope
;;; times the amount of partial envelopes. values may differ.
;;; 
;;; Arguments
;;; envelopes (rest)
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (mix-loop '(1 2 3) '(4 5) '( 6 7 8 9))
;;; -> (1 4 6 2 5 7 3 4 8 1 5 9)
;;;
;;; Last Modified
;;; 2019/10/05
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mix-loop (&rest envelopes)
  "mixes envelopes by looping all partial envelopes to the length
   of the longest partial envelope"
  (let ((min-len (apply 'max-length envelopes)))
    (apply 'merge-ls
	   (loop for env in envelopes collect
		(set-length min-len env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/mix-average
;;; Name
;;; mix-average
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; stretches all envelopes to the length of the longest one using
;;; linear interpolation. the average of all resulting envelopes
;;; for each index will be returned. the resulting envelope length 
;;; equals the length of the longest partial envelope.
;;; 
;;; Arguments
;;; envelopes (rest)
;;;
;;; Return Value
;;; list
;;;
;;; Example
;;; (mix-linpol '(1 2 3) '(4 5) '( 6 7 8 9)) -> ???
;;;
;;; Last Modified
;;; 2019/10/05
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mix-average (&rest envelopes)
  "mixes envelopes using linear interpolation and average values"
  (let* ((tar-len (max-length envelopes))
	 (envs-linpol 
	   (loop for env in envelopes
		 collect
		 (linpol tar-len env))))
    (loop for i from 0 to (1- tar-len)
	  collect
	  (average 
	   (loop for env in envs-linpol
		 collect
	       (float (nth i env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* envelopes/print-env
;;; Name
;;; print-env
;;;
;;; File
;;; envelopes.lisp
;;;
;;; Description
;;; prints the relations between envelope values in pulse notation
;;; 
;;; Arguments
;;; env
;;; resize-factor (optional): zoom in or out by factor
;;;
;;; Return Value
;;; t
;;;
;;; Example
;;; (print-env '(1 2 3 4 5) 1 nil) --> "x.x.x.x.x"
;;; (print-env '(1 2 3 4 5) 2 t) --> nil ("x..x..x..x..x")
;;;
;;; Last Modified
;;; 2019/10/28
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-env (env &optional (resize-factor 1) (output t))
  (format output "~a"
	  (with-output-to-string (str)
	    (loop for val in env
	       for next-val in (cdr env)
	       do
		 (format str "x~{~A~}" (set-length (round (abs (- (* resize-factor next-val) (* resize-factor val)))) ".")))
	    (format str "x"))))
