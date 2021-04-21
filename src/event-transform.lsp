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
;; event/event-transform.lsp                                                 2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/merge-events
;;; Name
;;; merge-events
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Merges two events into one:
;;; - Numeric properties are averaged
;;; - Subevents are all added to the new event, but not merged
;;; - the id of the returned event will always be nil
;;; - pathnames are not merged! The first pathname is taken
;;; - strings  are concatenated
;;;
;;; Arguments
;;; events
;;;
;;; Return Value
;;; event
;;;
;;; Example
#|
(merge-events (make-event :pitch 100 :amplitude (amp .5) 
                          :start-time (secs 10)) 
              (make-event :pitch 150 :amplitude .6 
                          :duration (secs 1) 
                          :render-modes 'isis :events (make-event)))
--> #<EVENT, START-TIME: #<10.0 SECS>, DURATION: #<1.0 SECS>, 
      RENDER-MODES: ISIS, PITCH: 125, AMPLITUDE: #<0.55 AMP>,
      events: 1>
|#
;;;
;;; Last Modified
;;; 2020/06/22
;;;
;;; Synopsis
(defun merge-events (&rest events)
;;; ****
  (setq events (flat events))
  (let ((new-event (make-event)))
    (labels ((get-set-vals (prop)
	       (let ((set-vals))
		 (loop for e in events do
		      (when (funcall prop e)
			(push (funcall prop e) set-vals)))
		 (reverse set-vals))))
      (loop for prop in (cc-get :event-slots)
	 do
	   (let* ((slotname (car prop))
		  (set-vals (get-set-vals slotname)))
	     (case (length set-vals)
	       ;; no value found
	       (0 nil)
	       ;; exactly one value found
	       (1 (funcall slotname new-event (car set-vals)))
	       (otherwise
		(case slotname
		  (id nil); ids will be ignored
		  ((render-modes events); render-modes will be appended
		   (funcall slotname new-event (flat set-vals)))
		  ((location expansion); spacial data will be averaged
		   (funcall slotname new-event
			    (apply #'mapcar #'average set-vals)))
		  (otherwise
		   (cond (; numeric units will be truely averaged
			  (member 'numeric-unit prop)
			  (funcall slotname new-event
				   (apply-with-unit
				    #'average
				    (get-set-vals (car prop)))))
			 ;; for text-slot, append all strings, 
			 ;; inserting spaces between them
			 ((member 'text prop)
			  (funcall slotname new-event
				   (apply #'concatenate
					  'string
					  (cons (car set-vals)
						(loop for val in
						     (cdr set-vals)
						   append
						     (list " " val))))))
			 ;; lists will be merged using merge-ls
			 ((member 'list prop)
			  (funcall slotname new-event
				   (apply #'merge-ls
					  (mapcar #'flat set-vals))))
			 ;; in any other case, the value of the first
			 ;; event to merge will be used.
			 (t
			  (funcall slotname new-event
				   (car set-vals))))))))))
      new-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/interpolate-events
;;; Name
;;; interpolate-events
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Interpolates two events in a given minimal amount of steps. See
;;; merge-event on infos on how interpolation is done. The amount of
;;; events returned is always a power-of-2 plus 1 (3, 5, 9, 17, etc.)
;;;
;;; Arguments
;;; e1, e2: two events
;;;
;;; Optional Arguments
;;; min-steps: the function will stop when at least this given amount
;;; of events was created. 
;;;
;;; Return Value
;;; list of events
;;;
;;; Last Modified
;;; 2020/11/25
;;;
;;; Synopsis
(defun interpolate-events (e1 e2 &optional (min-steps 2))
;;; ****
  (let ((return-ls (list e1 e2)))
    ;; repeat until we have at least min-step events
    (loop while (> min-steps (length return-ls)) do
      (setq return-ls
	    ;; always collect first event and merged event
	    (append
	     (loop for e1 in return-ls
		   for e2 in (cdr return-ls)
		   append
		   (list e1 (merge-events e1 e2)))
	     ;; at end, don't forget the last event...
	     (last return-ls))))
    ;; and return the list
    return-ls))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/dynamic-transform-events
;;; Name
;;; dynamic-transform-events
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Applies a procedure for transforming events on a list of events
;;; and their subevents. All numeric properties are then averaged
;;; between their original and transformed values using a given
;;; intensity-curve. The values of the intensity curve are arbitrary
;;; and will be mapped to fit the min and max values of the
;;; transformation.
;;; N.B.: Time information are important for this function!
;;; If they are not set yet, no gradual transformation can be made.
;;;
;;; Arguments
;;; event-proc: A procedure that takes a single event
;;; intensity-env: a list with values the determine the intensity
;;;                of the transformation
;;; events: the events (list or single event)
;;;
;;; Return Value
;;; events
;;;
;;; Example
#|
(dynamic-transform-events
   (lambda (e)
       (pitch e (u* (pitch e) 2)))
   '(0 0.1 0.3 0.5 0.8 0.9 0.95 1)
   (loop-make-event :start-time below 10 :duration 1 :pitch 1))
--> (#<EVENT, START-TIME: 0, DURATION: 1, PITCH: 1>
     #<EVENT, START-TIME: 1, DURATION: 1, PITCH: 1>
     #<EVENT, START-TIME: 2.0, DURATION: 1.0, PITCH: 1.1>
     #<EVENT, START-TIME: 3.0, DURATION: 1.0, PITCH: 1.3>
     #<EVENT, START-TIME: 4.0, DURATION: 1.0, PITCH: 1.5>
     #<EVENT, START-TIME: 5.0, DURATION: 1.0, PITCH: 1.8>
     #<EVENT, START-TIME: 6.0, DURATION: 1.0, PITCH: 1.8>
     #<EVENT, START-TIME: 7.0, DURATION: 1.0, PITCH: 1.9>
     #<EVENT, START-TIME: 8.0, DURATION: 1.0, PITCH: 1.95>
     #<EVENT, START-TIME: 9, DURATION: 1, PITCH: 2>)
|#
;;;
;;; Last Modified
;;; 2020/05/04
;;;
;;; Synopsis
(defun dynamic-transform-events (events
				 event-proc
				 intensity-env)
;;; ****
  (let* ((total-dur (get-total-duration events))
	 (env-steps (length intensity-env))
	 (time-per-env-step
	  (value (secs 
		  (if (<= env-steps 1)
		      total-dur
		      (u/ total-dur env-steps)))))
	 (factor-env (mapcar
		      (lambda (x)
			(shift-value
			 x
			 (apply #'min intensity-env)
			 (apply #'max intensity-env)
			 0 1))
		      intensity-env)))
    (when (= (value total-dur) 0)
      (cc-error 'DYNAMIC-TRANSFORM-EVENTS
	  "Can not perform dynamic transformation when~
           total duration is 0."))
    (loop for factor in factor-env
       for st by time-per-env-step
       for end from time-per-env-step by time-per-env-step
       do
	 (doevents (e events)
	   (let ((start-time (start-time e)))
	     (when
		   (and
		    (u>= start-time st)
		    (u< start-time end))
	       (let* ((transformed (clone e)))
		 (funcall event-proc transformed)
		 (loop for prop in (cc-get :event-slots) do
		      (when (and (member 'numeric-unit prop)
				 (funcall (car prop) e)
				 (funcall (car prop) transformed))
			(funcall (car prop) e
				 (u+
				  (u* (funcall (car prop) transformed)
				      factor)
				  (u* (funcall (car prop) e)
				      (abs (1- factor)))))))))))))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/change-times-by-factor
;;; Name
;;; change-times-by-factor
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Modifies all start-time and duration values of all given events
;;; and subevents by a factor, stretching or compressing their
;;; the total duration of the given structre.
;;;
;;; Arguments
;;; events: (list or single event  with subevents or comic)
;;; factor
;;;
;;; Return Value
;;; events
;;;
;;; Example
#|
(change-times-by-factor (loop-make-event :duration 1 :start-time from 0 to 2) 2)
-->
(#<EVENT, DURATION: #<2.0 SECS>, START-TIME: #<0.0 SECS (0.0 MSECS)>>
 #<EVENT, DURATION: #<2.0 SECS>, START-TIME: #<2.0 SECS>>
 #<EVENT, DURATION: #<2.0 SECS>, START-TIME: #<4.0 SECS>>)
|#
;;;
;;; Last Modified
;;; 2020/06/22
;;; 
;;; Synopsis
(defun change-times-by-factor (events factor)
;;; ****
  (cond ((numberp factor)
	 (doevents (e events)
	   (start-time e (u* (start-time e) factor))
	   (duration e (u* (duration e) factor))))
	((listp factor)
	 (let ((dur (/ (value (secs (get-total-duration events)))
		       (length factor))))
	   (loop for fac in factor
	      for st by dur
	      do
		(change-times-by-factor
		 (get-events-by-start-time events st (+ st dur))
		 fac))))
	(t (cc-error 'change-times-by-factor
	       "factor must be a number or a list of numbers")))
  events)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/distribute-values-by-proc
;;; Name
;;; distribute-values-by-proc
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Assigns given values to a specified event-slot. The given procedure
;;; (proc) must take an event and two values v1 and v2 and return t if
;;; v1 fits the event better then v2. The function tries to equally
;;; distribute all given values to events, while also finding
;;; a good value for each individual event.
;;;
;;; Arguments
;;; events: (list or single event with subevents or comic)
;;; values: A list of values that are all of a type expected by the
;;;         specified event-slot.
;;; event-slot: Name of the slot to fill, a symbol
;;; proc: A procedure used to sort events, like this:
;;;       (lambda (e v1 v2) ...) --> bool. Returns t if v1 is a better
;;;       match for the slot of e than v2.
;;;
;;; Return Value
;;; events
;;;
;;; Example
#|
(distribute-values-by-proc 
   (loop-make-event :duration from 1 to 5)
   '(1 2)
   'start-time 
   (lambda (e v1 v2) (if (u> v1 (duration e) v2) t)))
-->
(#<EVENT, DURATION: #<1.0 SECS>, START-TIME: 1>
 #<EVENT, DURATION: #<2.0 SECS>, START-TIME: 2>
 #<EVENT, DURATION: #<3.0 SECS>, START-TIME: 1>
 #<EVENT, DURATION: #<4.0 SECS>, START-TIME: 2>
 #<EVENT, DURATION: #<5.0 SECS>, START-TIME: 1>)
|#
;;;
;;; Last Modified
;;; 2020/11/10
;;; 
;;; Synopsis
(defun distribute-values-by-proc (events values event-slot proc)
;;; ****
  "Assigns given values to a specified event-slot. The given procedure
  (proc) must take an event and two values v1 and v2 and return t if
  v1 fits the event better then v2. The function tries to equally
  distribute all given values to events, while also finding a good
  value for each individual event."
  ;; check if we have at least 2 values:
  (unless (and (listp values) (> (length values) 1))
    (cc-error 'distribute-values-by-proc
  	"Values must be a list with at least 2 elements.~%~
         Not: ~a" values))
  ;; check if the given event slot is legal:
  (unless (member event-slot (cc-get :event-slots) :key #'first)
    (cc-error 'distribute-values-by-proc
	"The slotname ~a could not be found in the event-class."
      event-slot))
  ;; check if all values have a legal type:
  (let ((types (second (cc-get :event-slots event-slot))))
    (unless
	(all? (lambda (v) (type-or v types))
	      values)
      (cc-error 'distribute-values-by-proc
	  "Not all elements in values match the expected type for ~
           event-slot ~a.~%~
           expected types: ~a~%values: ~a"
	event-slot values types)))
  ;; if everything is fine, start to distribute values to events:
  (let* (;; lists if value and counter
	 (vals-and-counts
	   (loop for v in values collect (list v 0)))
	 ;; collect best matches here:
	 (best-vals-and-counts nil)
	 ;; store each final match here:
	 (match nil)
	 ;; The amount of "good" matches: Always half the total amount
	 (best-amount (max (ceiling (/ (length values) 2)) 2)))
    (doevents (e events)
      ;; sort vals by best matches. We want to exclude the
      ;; not-so-good-ones first!
      (sort vals-and-counts
	    (lambda (v1 v2) (funcall proc e v1 v2)) :key #'first)
      ;; get the first half of the list, the "better" half:
      (setq best-vals-and-counts
	    (subseq vals-and-counts 0 best-amount))
      ;; sort it by amount of occurancy to target equal distribution:
      (sort best-vals-and-counts #'< :key #'second)
      (setq match (caar best-vals-and-counts)) ; the match only!
      (incf (second (assoc match vals-and-counts)))
      (setf (slot-value e event-slot) match)))
  events)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/inverse-values-in-slot
;;; Name
;;; inverse-values-in-slot
;;;
;;; File
;;; event-transform.lsp
;;;
;;; Description
;;; Inverses all numeric values in a given slot for a list of events
;;; (recursively!). The range for the inversion will be min->max the
;;; given value range.
;;;
;;; Arguments
;;; slot-name: A legal name for a slot of the event-class (symbol)
;;; events: (list or single event with subevents or comic)
;;;
;;; Return Value
;;; events
;;;
;;; Example
#|
(inverse-values-in-slot 'start-time
   (#<EVENT, PITCH: 100, START-TIME: #<0.0 SECS (0.0 MSECS)>>
    #<EVENT, PITCH: 150, START-TIME: #<2.0 SECS>>
    #<EVENT, PITCH: 200, START-TIME: #<4.0 SECS>>))
--> start-time
(#<EVENT, PITCH: 100, START-TIME: #<4.0 SECS>>
 #<EVENT, PITCH: 150, START-TIME: #<2.0 SECS>>
 #<EVENT, PITCH: 200, TART-TIME: #<0.0 SECS (0.0 MSECS)>>)
|#
;;;
;;; Last Modified
;;; 2021/04/20
;;; 
;;; Synopsis
(defun inverse-values-in-slot (slot-name events)
;;; ****
  ;; find first unit:
  (let ((unit))
    (doevents (e events)
      (when (unit-p (funcall slot-name e))
	(setq unit (type-of (funcall slot-name e)))
	(return)))
    ;; find minimum and maximum slot value:
    (let ((min) (max))
      (doevents (e events)
	(let ((val (value e unit slot-name)))
	  (when (numberp val)
	    (when (or (not min) (< val min))
	      (setq min val))
	    (when (or (not max) (> val max))
	      (setq max val)))))
      ;; function can only work if min and max are set now
      (if (and min max)
	  ;; inverse values:
	  (doevents (e events)
	    (let ((val (value e unit slot-name)))
	      (when (numberp val)
		(funcall slot-name e
			 (funcall unit (- max (- val min)))))))
	  ;; if no inversion possible, print a warining:
	  (cc-warn 'inverse-values-in-slot
		   "Could not inverse values in ~a events ~%~
                  for slot ~a: No min and/or max values found."
		   (count-events events) slot-name))))
  events)

	    

;;(defun subdivide-event (event)
;;--> Ein event mit x subevents füllen, die es im render ersetzen (ntolen quasi)

;; (defun multiply-events

;; (defun make-event-cloud

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF event-transform.lsp
