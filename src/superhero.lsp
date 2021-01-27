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
;; event/superhero.lsp                                                       2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/superhero
;;; Name
;;; superhero
;;;
;;; File
;;; superhero.lsp
;;;
;;; Description
;;; A superhero is an object that holds code (and variable names
;;; and definitions) to be run once for each event in a comic.
;;; It can be defined using make-superhero and executed using
;;; call-superhero. It can be used to capture an iterative
;;; algorithmic process, that will be usefull to apply on a
;;; comic in different situations and projects, e.g. an
;;; auto-completion algorithm for specific slots of the events.
;;;
;;; Slots
;;; outer-loop, locals, code, event-var, parent-var,
;;; return-var
;;; For more info, see make-superhero.
;;;
;;; Last Modified
;;; 2020/06/25
;;;
;;; Synopsis
(defclass superhero (named-object)
  (;; outer-loop, controlling amount
   ;; of times the superhero iterates
   ;; through the entire comic
   (outer-loop
    :initarg :outer-loop
    :initform '(repeat 1))
   ;; additional locals, defined in a let-manner
   (locals
    :initarg :locals
    :initform nil)
   ;; code to run once for each event
   (code
    :initarg :code
    :initform nil)
   ;; variable holding current event 
   (event-var
    :initarg :event-var
    :initform 'e)
   ;; variable holding current-events parent
   (parent-var
    :initarg :parent-var
    :initform 'p)
   ;; variable to return at the very end
   (return-var
    :initarg :return-var
    :initform 'r)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* superhero/make-superhero
;;; Name
;;; make-superhero
;;;
;;; File
;;; superhero.lsp
;;;
;;; Description
;;; Creates an instance of the superhero-class.
;;; Adds the object to +cc-data+ / :superheros,
;;; accessible by its name.
;;;
;;; Arguments
;;; N.B.: make-superhero is a macro, so all args without quotes!
;;; - name (symbol)
;;; - code: Lisp-form to evaluate once per event per iteration.
;;;
;;; Optional Arguments
;;; - outer-loop: One or more loop forms, controlling the amount
;;;   of iterations through the entire comic. Can be set to nil,
;;;   which will result in (loop do ...), which is quite dangerous.
;;;   Default: (repeat 1), for 1 iteration only.
;;; - locals: A set of local variables for any purpose, defined in
;;;   a let-manner.
;;; - event-var: Variable to hold current event, default: e.
;;; - parent-var: Variable to hold current events parent, default: p.
;;; - return-var: Variable to return at the end of all iterations,
;;;   default: r.
;;;
;;; Return Value
;;; Superhero
;;;
;;; Last Modified
;;; 2020/06/26
;;;
;;; Synopsis
(defmacro make-superhero (name
			  code
			  &key
			    (outer-loop '(repeat 1))
			    locals
			    (event-var 'e)
			    (parent-var 'p)
			    (return-var 'r))
;;; ****
  (let ((instance
	 (make-instance 'superhero
			:name name
			:outer-loop outer-loop
			:locals locals
			:code code
			:event-var event-var
			:parent-var parent-var
			:return-var return-var)))
    ;; add superhero to +cc-data+
    (cc-set :superheros (acons name instance (cc-get :superheros)))
    instance))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* superhero/call-superhero
;;; Name
;;; call-superhero
;;;
;;; File
;;; superhero.lsp
;;;
;;; Description
;;; Calls a superhero to modify the given comic.
;;;
;;; Arguments
;;; N.B.: call-superhero is a macro, so all args without quotes!
;;; - superhero (superhero-instance or its name as symbol)
;;; - comic
;;;
;;; Optional Arguments
;;; All key-args specified in the lambda-list slot of the superhero.
;;;
;;; Return Value
;;; return-var of the superhero
;;;
;;; Last Modified
;;; 2020/06/26
;;;
;;; Synopsis
(defmacro call-superhero (superhero comic)
;;; ****
  (let* ((superhero (if (typep superhero 'superhero)
			superhero
			(cc-get :superheros superhero)))
	 (e (slot-value superhero 'event-var))
	 (p (slot-value superhero 'parent-var))
	 (r (slot-value superhero 'return-var))
	 (locals (slot-value superhero 'locals)))
    `(let* ((,r nil)
	    (comic ,comic)
	    ,@locals)
       (declare (ignorable comic))
       (loop ,@(slot-value superhero 'outer-loop) do
	  (labels ((helper (,p events)
		     (declare (ignorable p))
		     (loop for ,e in (flat events) do
			  ,(slot-value superhero 'code) ; execute event code
			  (when (events ,e)
			    ;; recursive call, next generation
			    (helper ,e (events ,e))))))
	    (helper nil ,comic))
	  finally (return ,r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* placed-object/get-space
;;; Name
;;; get-space
;;;
;;; File
;;; superhero.lsp
;;;
;;; Description
;;; Returns a placed-objects with location and expansion slots set.
;;; It represents the total space that all given placed-objects
;;; fill. 
;;;
;;; Arguments
;;; - placed-objects: a flat list of placed-objects
;;;
;;; Optional Arguments (&key)
;;; - ensure-equal-dimensions: Should all given objects be set to
;;;   the maximum amount of dimensions first? Default: t.
;;;   When calling this function lots of times in iterations,
;;;   it may make sense to set this to nil.
;;;   When equal dimensions are set, the given objects are directly
;;;   modified.
;;;
;;; Return Value
;;; placed-object
;;;
;;; Example
#|
(get-space (list (make-protagonist :sound :location '(1 2)
                                   :expansion '( 8 2)) 
                 (make-event :location 1 :expansion 2)))
--> placed-object, EXPANSION = (8 3), LOCATION = (1 3/2)
|#
;;;
;;; Last Modified
;;; 2020/06/27
;;;
;;; Synopsis
(defun get-space (placed-objects
		  &key
		    (ensure-equal-dimensions t))
;;; ****
  "Returns a placed-object representing the total space
   filled placed-objects."
  (when ensure-equal-dimensions
    ;; set all dimensions to max dimension
    (loop for obj in placed-objects
       maximize (get-dimensions obj) into dims
       finally (loop for obj in placed-objects
		  do (set-dimensions obj dims))))
  ;; add all subevents to list of placed-objects
  (loop for obj in placed-objects do
       (when (eventp obj)
	 (doevents (e (events obj))
	   (push e placed-objects))))
  ;; only keep objects with set data
  (setq placed-objects
	(loop for obj in placed-objects
	   when (and (location obj) (expansion obj))
	   collect obj))
  (let (;; initalise outer-bounds
	(outer-bounds
	 (loop
	    for l in (location (car placed-objects))
	    for e in (expansion (car placed-objects))
	    collect
	      (list (min (+ l (* 1/2 e)) (- l (* 1/2 e)))
		    (max (+ l (* 1/2 e)) (- l (* 1/2 e)))))))
    ;; get maximum outer-bounds for each dimension
    (loop for obj in (cdr placed-objects) do
	 (setq outer-bounds
	       (loop for (min max) in outer-bounds
		  for l in (location obj)
		  for e in (expansion obj)
		  collect
		    (list (min min (+ l (* 1/2 e)) (- l (* 1/2 e)))
			  (max max (+ l (* 1/2 e)) (- l (* 1/2 e)))))))
    (loop for (min max) in outer-bounds
       collect (float (- max min)) into exp
       collect (float (+ min (/ (- max min) 2))) into loc
       finally (return
		 (make-instance 'placed-object
				:expansion exp
				:location loc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****S* superhero/space-superhero
;;; Name
;;; space-superhero
;;;
;;; File
;;; superhero.lsp
;;;
;;; Description
;;; The space-superhero sets location- and expansion-slots of all
;;; events without further subevents. If no locations and expansions
;;; are set at all, all values will default to '(0). Otherwise, the
;;; superhero calculates values by looking at the total space given,
;;; as well as values already set.
;;;
;;; Return Value
;;; boolean, indicating if all location and expansion slots
;;; of events without subevents are set.
;;;
;;; Example
#|
(call-superhero space-superhero my-comic) --> t
|#
;;;
;;; Last Modified
;;; 2020/06/27
;;;
;;; ****
(make-superhero space-superhero
		(unless (events e) ; only last children
		  (let ((treatment nil))
		    ;; find location
		    (cond
		      ;; if nothing is set at all, simply set
		      ;; all values to 0.
		      ((null (location total-space))
		       (location e '(0))
		       (setq treatment t))
		      ;; if a location is set, reset average-loc
		      ((location e)
		       (setq average-loc
			     (if average-loc
				 (loop for l1 in average-loc
				    for l2 in (location e)
				    collect (/ (+ l1 l2) 2))
				 (location e))))
		      ;; if an average location is given,
		      ;; find a location between average loc
		      ;; and center of total space
		      (average-loc
		       (location e (loop for l1 in average-loc
				      for l2 in (location total-space)
				      with fac = (/ events-to-treat
						       events-to-treat-total)
				      collect (+ (* l1 fac) (* l2 (- 1 fac)))))
		       (setq treatment t))
		      ;; if no average-loc is set yet, set the
		      ;; event to the center of the space
		      (t (location e (location total-space))
			 (setq treatment t)))
		    ;; find expansion
		    (cond
		      ;; if nothing is set at all, simply set
		      ;; all values to 0.
		      ((null (expansion total-space))
		       (expansion e '(0))
		       (setq treatment t))
		      ;; if an expansion is set, reset average-exp
		      ((expansion e)
		       (setq average-exp
			     (if average-exp
				 (loop for e1 in average-exp
				    for e2 in (expansion e)
				    collect (/ (+ e1 e2) 2))
				 (expansion e))))
		      ;; if an average expansion is given,
		      ;; find an expansion between average-exp
		      ;; and expansion of total space devided
		      ;; by amount of events to treat
		      (average-exp
		       (expansion e (loop for e1 in average-exp
				       for e2 in (expansion total-space)
				       with fac = (/ events-to-treat
						     events-to-treat-total)
				       collect
					 (+ (* e1 fac)
					    (* (/ e2 events-to-treat-total)
					       (- 1 fac)))))
		       (setq treatment t))
		      ;; if no average-exp is set yet,
		      ;; set expansion to 0
		      (t (expansion e 0)
			 (setq treatment t)))
		    (when treatment (decf events-to-treat))
		    (when (zerop events-to-treat) (setq r t))))
		:locals
		((average-loc nil)
		 (average-exp nil)
		 (total-space
		  (get-space
		   (flat comic)))
		 (events-to-treat
		  (let ((r 0))
		    (doevents (e comic)
		      (unless (or (location e)
				   (expansion e)
				   (events e))
			(incf r)))
		    r))
		 (events-to-treat-total
		  events-to-treat)))
		 
		
		    
		    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
