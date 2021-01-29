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
;; event/event.lsp                                                           2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* cc/event
;;; Name
;;; event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Events are the core of composing with comic: There slots hold properties
;;; that will be passed to render-modes to create the final piece. 
;;; Property-slots can also be added to the class using (add-unit).
;;;
;;; Purpose:
;;; - Definition of the event-class
;;; - Definition associated creation and accessor functions
;;; - function for dynamically adding new slots to the class and redefining it
;;;   (add-unit)
;;;
;;; Classes
;;; event
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (cc-get-event-slot-default-unit 'pitch 'number) -> hz
(defun cc-get-event-slot-default-unit (slot-name type)
  "Returns a list of lists containing a type and a default unit 
   for all types with default units that match the specified slot."
  (loop for tp in (cc-get :event-slots slot-name) do
       (when (and (listp tp)
		  (eq (car tp) type))
	 (return (second tp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-prepare-value-for-event-slot (value slot-name)
  "Converts value to default unit if specified for this specific 
   type or returns the value if no defaut unit is specified
   or returns nil if the value does not match any of the types for
   this slot."
  (when (eq slot-name 'events)
    (setq value (flat value)))
  (loop for type in (cc-get-event-slot-types slot-name) do
       (when (typep value type)
	 (let ((unit
		(cc-get-event-slot-default-unit slot-name type)))
	   (return (if unit (convert value unit) value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-get-direct-event-slots ()
  "Get all direct slots of the event class:
    excludes slots inherited from placed-object."
  (let ((direct-slots nil))
    (loop for slot in (cc-get :event-slots) do
      (unless (member (car slot) '(name location expansion render-modes))
	(push slot direct-slots))
	  finally (return direct-slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc-make-event-class ()
  "Dynamically (re-)define the event class and (make-event)" 
  (let* ((event-slots (cc-get :event-slots))
	 (eval-ls
	  (list 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* event/event
;;; Name
;;; event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Definition of the event-class. Everything organised in time is
;;; is an event, including the entire comic, which has the
;;; event-class as its super-class.	   
;;;
;;; Slots
;;; name, id, events, duration, start-time, pitch, amplitude
;;; location, expansion, notation, text, soundfile, videofile
;;; render-modes
;;; N.B.: Slots can be added dynamically. For an up-to-date list
;;; of slot-names and value-types, run (cc-get :event-slots).
;;;
;;; Last Modified
;;; 2019/12/21
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   `(defclass event (placed-object named-object)
	      ,(loop for slot in (cc-get-direct-event-slots) collect
		    (list (first slot)
			  :initarg (make-keyword (first slot))
			  :initform nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/make-event
;;; Name
;;; make-event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Creates an event. All values given for event-slots will be
;;; type-checked.
;;;
;;; Arguments
;;; All event-slot-names as &key-arguments. For a complete list,
;;; refere to +cc-data+ or run (cc-get :event-slots).
;;;
;;; Return value
;;; event
;;;
;;; Example
;;; (make-event) --> <#EVENT>
;;;
;;; Last Modified
;;; 2019/12/29
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   `(defun make-event ,(append
				'(&key)
				(loop for slot
				   in event-slots
				   collect
				   (first slot))
				'(&allow-other-keys))
	      (labels (;; checks if the specified value
		       ;; matches the types listed in
		       ;; +cc-data+
		       ;; also sets non-property-values to
		       ;; default units
		       (ok? (val slot-name)
			 (when val
			   (let ((the-val
				  (cc-prepare-value-for-event-slot
				   val slot-name)))
			     (if the-val
				 the-val
				 (cc-error 'MAKE-EVENT
				     "The value ~
                            ~a does not match~%any expected ~
                            type for event-slot ~a ~a."
				   val slot-name
				   (flat (cc-get-event-slot-types slot-name))))))))
		,(append '(make-instance
			   'event)
			 (loop for slot in event-slots
			    append
			      (list (make-keyword (first slot))
				    `(ok? ,(first slot)
					  ',(first slot))))))))))
    (loop for def in eval-ls do
	 (eval def)))
  t)
;; initial call to the macro when loading comic:
(cc-make-event-class)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-slots append ((event event))
  (loop for slot in (cc-get-direct-event-slots)
	collect
	(car slot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ACCESSOR-FUNCTIONS
;;; getting and setting values with one single function and with an
;;; implicit (setf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop for slot in (cc-get :event-slots) do
     (let* ((slot-name (first slot)))
       (unless
	   (member slot-name
		   (cons 'name
			 (get-slots (make-instance 'placed-object))))
	 (eval `(cc-make-accessor ,slot-name
				  event
				  (quote ,(cdr slot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((event event) stream)
  (when (eq (type-of event) 'event) ; only if type is not comic!
    (format stream "#<EVENT")
    (when (name event)
      (format stream ": ~a" (name event)))))

(defmethod print-object ((obj event) stream)
  (loop for slot in (get-slots obj)
     do
       (let* ((val (funcall slot obj)))
	 ;; do not always print all events recursively
	 ;; for an entire recursive list of events, print-events
	 ;; can be used.
	 ;; Name will be printed by comic-before-method.
	 (when (and val (not (member slot '(name events))))
	   ;; do not print id-slot for comics (always 0)
	   (unless (and (comicp obj) (eq slot 'id))
	     (format stream ", ~a: ~a" slot val)))))
  (when (events obj)
    (format stream ", EVENTS: ~d" (count-events obj)))
  (format stream ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/add-event-slot
;;; Name
;;; add-event-slot
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Adds a new unit-slot to (cc-get :event-slots) and
;;; redefines (make-event) and all accessor functions.
;;;
;;; Arguments
;;; slot-name: name of the new slot, a symbol
;;; list-of-types (optional, default: t): a list of type names
;;;               that is checked
;;;
;;; Return Value
;;; t
;;;
;;; Example
;;; (add-event-slot 'dance-move (symbol string))
;;;
;;; Last Modified
;;; 2020/01/06
;;;
;;; Synopsis
(defun add-event-slot (slot-name &optional default-unit (list-of-types '(t)))
;;; ****
  (if (member
       slot-name
       (loop for prop in (cc-get :event-slots)
       	  collect (first prop)))
      (cc-warn 'ADD-PROPERTY-SLOT
	       "The slot ~a already ~
              exists in the event-class. ~%~
             Use cc-reload to reset all slots." slot-name)
      (progn
	(cc-set :event-slots
		(append
		 (cc-get :event-slots)
		 (list (cons slot-name
			     (cons default-unit list-of-types)))))
	(eval '(macroexpand (cc-make-event-class)))
	(eval `(cc-make-accessor ,slot-name event (quote ,list-of-types)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/print-events
;;; Name
;;; print-events
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Prints a complete list of event and all subevents.
;;;
;;; Arguments
;;; event
;;;
;;; Optional Arguments
;;; stream: The stream to print to. (default: t)
;;;
;;; Return value
;;; nil
;;;
;;; Last Modified
;;; 2020/06/20
;;;
;;; Synopsis
(defun print-events (event &optional (stream t))
;;; ****
  (labels ((print-events (events tab)
	     (unless (listp events)
	       (setq events (list events)))
	     (loop for event in events
		do
		  (if (> tab 0)
		      (format stream (format nil "~~&~~~dT" tab))
		      (format stream "~&"))
		  (format stream "#<EVENT")
		  (loop for slot in (cc-get :event-slots)
		     do
		       (let* ((key (first slot))
			      (val (funcall key event)))
			 (when (and val (not (eq key 'events)))
			   (if (listp val)
			       (format stream ", ~a: (~{~a~^ ~})" key val)    
			       (format stream ", ~a: ~a" key val)))))
		  (format stream ">")
		  (when (events event)
		    (print-events (events event) (1+ tab))))))
    (print-events event 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/value
;;; Name
;;; event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Accesses the value of a unit-object stored in the slot of an 
;;; event and returns it, converted to the specified unit.
;;;
;;; Arguments
;;; object: The object
;;; unit: unit to convert to. error is signaled if conversion fails.
;;; unit-name: The name of the unit to access. Only used
;;;                object is an event
;;;
;;; Example
#|
(value (make-event :pitch (midinote 60)) 'hz 'pitch) -> 261.62555
|#
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defmethod value ((object event) &optional unit slot-name)
;;; ****
  (if slot-name
      (value (slot-value object slot-name) unit)
      (cc-error 'value "Value is called on an event, but no ~
                        unit-name is specified.~%~
                        ~a" object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/eventp
;;; Name
;;; eventp
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; returns t if the specified object is an event
;;;
;;; Arguments
;;; obj: any object
;;;
;;; Return Value
;;; boolean
;;;
;;; Example
;;; (eventp my-event) --> t
;;;
;;; Last Modified
;;; 2019/12/21
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eventp (obj)
  "checks if an object is an event"
  (typep obj 'event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/clone
;;; Name
;;; clone
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Method for cloning events (including comics). Also clones
;;; all events in the events-slot recursively.
;;;
;;; Arguments
;;; event: an event-object
;;;
;;; Return Value
;;; the clone
;;;
;;; Example
;;; (clone my-event) --> clone-of-my-event
;;;
;;; Last Modified
;;; 2020/01/31
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric clone (obj))

(defmethod clone ((event event))
  "method for cloning events (including comics).
   also clones all events in the events-slot recursively."
  (let ((clone (make-instance (type-of event)))
	(slots (get-slots event)))
    (loop for slot in slots do
      (setf (slot-value clone slot) (slot-value event slot)))
    (let ((events-in-clone
	    (events clone)))
      (when events-in-clone
	(events clone
		(loop for ev in (flat events-in-clone)
		      collect
		      (clone ev))))
      clone)))

(defmethod clone (ls)
  (if (listp ls)
      (loop for e in ls collect
	   (clone e))
      ls))

(defgeneric add-events (event events &optional id))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/add-events
;;; Name
;;; add-events
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Adds events to an event.
;;;
;;; Arguments
;;; event: the event to add events to
;;; events: an event or a list of events to add
;;;
;;; Optional Arguments
;;; id: If event is a comic and id is set, the events are added
;;; to the event with given ID.
;;;
;;; Return Value
;;; boolean (t if events were added, else nil)
;;;
;;; Example
#|
(add-events my-event list-of-events) --> t
|#
;;;
;;; Last Modified
;;; 2020/02/01
;;;
;;; Synopsis
(defmethod add-events ((event event) events &optional id)
;;; ****
  "add events to the event-slot of an event."
  (declare (ignore id))
  (unless (type-or events '(event list))
    (cc-type-error 'ADD-EVENTS events '(or EVENT LIST)))
  (let ((slot-val (slot-value event 'events)))
    (ensure-list slot-val)
    (ensure-list events)
    (setf (slot-value event 'events)
	  (append slot-val events))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/loop-make-event
;;; Name
;;; loop-make-event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Creates multiple events and returns them in a list.
;;; The clauses of the loop-macro can be used like this:
;;; :pitch from 0 to 10 :duration in '(1 2 3), etc.
;;; Static values can also be supplied (:pitch (hz 100)).
;;; The loop terminates when the first of its clauses is
;;; finished (as usually).
;;;
;;; N.B.: This function can not yet handle unit-objects as values
;;;       in event-slots!
;;;
;;; Arguments
;;; &body keywords-and-forms
;;;
;;; Return Value
;;; list of events
;;;
;;; Example
#| (loop-make-event :duration from 1 to 3)
-> ([EVENT, dur: 1]
[EVENT, dur: 2]
[EVENT, dur: 3])
(loop-make-event :duration in '(1 2 3))
-> ([EVENT, dur: 1]
[EVENT, dur: 2]
[EVENT, dur: 3])
(loop-make-event :duration in '(1 2 3) :pitch to 1000)
-> ([EVENT, pitch: 0, dur: 1]
[EVENT, pitch: 1, dur: 2]
[EVENT, pitch: 2, dur: 3])
(loop-make-event :pitch (hz 100))
-> ([EVENT, pitch (HZ 100)])
(loop-make-event)
-> ([EVENT])
|#
;;;
;;; Errors
;;; (loop-make-event :duration nil for n below 10) --> ERROR
;;; (loop-make-event for n below 10) --> ERROR
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
(defmacro loop-make-event (&body keywords-and-forms)
  "Some sugar around loop for creating lots of comic-events"
;;; ****
  ;; ADD SUPPORT FOR UNITS!
  ;; E.G.: for :pitch from (hz 100) to (hz 200) by 10
  (let* ((all-keys
	  (loop for slot in (cc-get :event-slots)
	     collect
	       (make-keyword (first slot))))
	 ;; split keyswords-and-forms into sublists with
	 ;; arguments to (make-event) as car
	 (keys-and-forms
	  (apply
	   'sublists-by-keywords
	   keywords-and-forms
	   all-keys))
	 ;; a little ugly, but we need iteration
	 ;; variables (more than slots in comic-event!)
	 (vars
	  '(a b c d e f g h i j k l m n o p q r s
	    t u v w x y z aa bb cc dd ee ff gg hh
	    ii jj kk ll mm nn oo pp qq rr ss tt uu
	    vv ww xx yy zz))
	 ;; will both be filled with code in let*-body...
	 (loop-code)
	 (args))
    ;; ERROR-handling:
    ;; if the first keyword is not a keyword-argument
    ;; for make-event, (sublists-by-keywords) will skip the
    ;; first list argument and return the value unnested.
    ;; Because of this, (listp (car keys-and-forms)) will
    ;; test if everything is ok. All other errors will be
    ;; handled by (loop) directly.
    (unless (listp (car keys-and-forms))
      (cc-error 'LOOP-MAKE-EVENT "~a is not a proper keyword-argument ~%~
              for make-event." (car keys-and-forms)))
    ;; build loop-code and static arguments:
    (loop for key-form in keys-and-forms
       for n from 0 do
	 (if (> (length key-form) 2)
	     (progn
	       (setq loop-code
		     (append
		      loop-code
		      '(for)
		      (list (nth n vars))
		      (cdr key-form)))
	       (setq args
		     (cons (car key-form)
			   (cons (nth n vars)
				 args))))
	     (setq args
		   (append key-form args))))
    ;; make function-body:
    (if (null loop-code)
	`(list (make-event ,@args))
	`(loop ,@loop-code collect
	    (make-event ,@args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/permutate-make-event
;;; Name
;;; permutate-make-event
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Creates multiple events and returns them in a list.
;;; For all arguments in lists, it iterates over the items
;;; in that list to create all possible permutations of
;;; the given values.
;;;
;;; Arguments
;;; &body keywords-and-forms
;;;
;;; Return Value
;;; list of events
;;;
;;; Example
#| (loop-permutate-event :duration '(1 2 3) :start-time '(5 4 5))
-> ([EVENT, START-TIME: 5, DURATION: 1]
[EVENT, START-TIME: 5, DURATION: 2]
[EVENT, START-TIME: 5, DURATION: 3]
[EVENT, START-TIME: 4, DURATION: 1]
[EVENT, START-TIME: 4, DURATION: 2]
[EVENT, START-TIME: 4, DURATION: 3]
[EVENT, START-TIME: 5, DURATION: 1]
[EVENT, START-TIME: 5, DURATION: 2]
[EVENT, START-TIME: 5, DURATION: 3])
|#
;;;
;;; Last Modified
;;; 2020/04/22
;;;
;;; Synopsis
(defun permutate-make-event (&rest args)
  "takes list-values for key-args to make-event and outputs
   a list of all possible permutations of events with given
   values."
;;; ****
  (let* ((all-keys
	  (loop for slot in (cc-get :event-slots)
	     collect
	       (make-keyword (first slot))))
	 ;; split keyswords-and-forms into sublists with
	 ;; arguments to (make-event) as car
	 (args-ls
	  (apply
	   'sublists-by-keywords
	   args
	   all-keys))
	 (events (list (make-event))))
    (loop for (key vals) in args-ls do
	 (ensure-list vals)
	 (setq events
	       (loop for val in vals append
		    (loop for event in events collect
			 (apply #'make-event key val
				(cc-get-event-creation-args event))))))
    events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/doevents
;;; Name
;;; doevents
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Executes its body for each event in events. Also applies for
;;; all subevents recursively. Inside the body code, var is bound
;;; to the current event. The return value is t, but can
;;; conditionally be turned to nil using (return nil) in the body.
;;;
;;; Arguments
;;; var: variable for current event in loop-cycle
;;; events: the events, can be a list of events or a single event
;;;         (or comic)
;;; loop-body (&body): The lisp-forms to execute
;;;
;;; Return Value
;;; t
;;;
;;; Last Modified
;;; 2020/02/06
;;;
;;; Synopsis
(defmacro doevents ((var events) &body body)
;;; ****
  "recursively loop through events and run body each iteration."
  `(labels ((helper (events)
	      (loop for ,var in (flat events) do
		   (progn
		     ,@body
		     (when (slot-value ,var 'events)
		       (let ((return (helper (slot-value ,var 'events))))
			 (unless (eq return t)
			   (return return)))))
		 finally (return t))))
     (helper ,events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/count-events
;;; Name
;;; count-events
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Returns the amount of subevents of an event.
;;;
;;; Arguments
;;; event: the event
;;;
;;; Return Value
;;; number
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; Synopsis
(defun count-events (event)
  "counts the number of subevents of an event."
;;; ****
  (let ((count -1))
    (doevents (e event)
      (incf count))
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/get-total-duration
;;; Name
;;; get-total-duration
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Returns the total duration of an event (with subevents), a comic
;;; or a list of events. 
;;;
;;; Arguments
;;; event: the event
;;;
;;; Return Value
;;; a unit object (secs)
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; Synopsis
(defun get-total-duration (&rest events)
  "determine the total duration of a comic or list of events"
;;; ****
  (let ((total-dur 0))
    (doevents (e events)
      (let* ((st (if (start-time e)
		     (value e 'secs 'start-time)
		     0))
	     (dur (if (duration e)
		      (value e 'secs 'duration)
		      0))
	     (end (+ st dur)))
	(when (> end total-dur)
	  (setq total-dur end))))
    (secs total-dur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/get-events-by-start-time
;;; Name
;;; get-events-by-start-time
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Returns all events with a start-time greater than or equal to
;;; time1 and lesser than or equal to time2. If time2 is not set,
;;; It will default to time1, meaning: Only events with a start-time
;;; equal to time1 will be returned.
;;;
;;; N.B.: The function goes through all subevents, but returns a
;;;       flat list of all matches. As it will only look at the
;;;       slot values of each single event, this can lead to
;;;       unexpected behaviour when the events are not flattened.
;;;
;;; Arguments
;;; events: A list of events, a single event or a comic
;;; time1: First start-time to match
;;;
;;; Optional Arguments:
;;; time2: Last start-time to match
;;;
;;; Return Value
;;; list of events
;;;
;;; Last Modified
;;; 2020/06/22
;;;
;;; Synopsis
(defun get-events-by-start-time (events time1 &optional time2)
  "return all events the start at time1 or between time1 and time2"
;;; ****
  (unless time2 (setq time2 time1))
  (let* ((result nil))
    (doevents (e events)
      (when (and (start-time e)
		 (u>= (start-time e) time1)
		 (u<= (start-time e) time2))
	(push e result)))
    (reverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/filter-events
;;; Name
;;; filter-events
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Filter events by a given predicate/function.
;;; Only if (funcall predicate e) returns t, e is returned.
;;; The function is applied on all subevents,
;;; but returnes a flat list!
;;;
;;; Arguments
;;; predicate
;;; rest: events
;;;
;;; Return Value
;;; a list of events
;;;
;;; Example
#|
(filter-events 'pitch (make-event :pitch (hz 100)) (make-event))
 --> (#<EVENT, PITCH: #<100.0 HZ>>)
|#
;;;
;;; Last Modified
;;; 2020/05/04
;;;
;;; Synopsis
(defun filter-events (predicate &rest events)
;;; ****
  (let ((result nil))
    (doevents (e events)
	      (when (funcall predicate e)
		(push e result)))
    (reverse result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/get-events-in-time-range
;;; Name
;;; get-events-in-time-range
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Returns all events in a given time range, even if they start
;;; earlier or last longer.
;;;
;;; N.B.: The function goes through all subevents, but returns a
;;;       flat list of all matches. As it will only look at the
;;;       slot values of each single event, this can lead to
;;;       unexpected behaviour when the events are not flattened.
;;;
;;; Arguments
;;; events: A list of events, a single event or a comic
;;; t1, t2: two times, either numbers or unit-objects
;;;
;;; Return Value
;;; list of events
;;;
;;; Last Modified
;;; 2020/11/25
;;;
;;; Synopsis
(defun get-events-in-time-range (t1 t2 &rest events)
  "Returns all events in a given time range, even if they start
  earlier or last longer."
;;; ****
  (apply
   #'filter-events
   (lambda (e)
     (let* ((st (start-time e))
	    (end (u+ st (duration e))))
       (or (u< t1 st t2)
	   (u< t2 st t1)
	   (u< t1 end t2)
	   (u< t2 end t1)
	   (u< st t1 end)
	   (u< st t2 end))))
   events))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/get-dimensions
;;; Name
;;; get-dimensions
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Returns the maximum amount of dimensions an event has
;;; (= length of lists in location and expansion slots).
;;; All subevents are considered.
;;;
;;; Arguments
;;; obj: an event
;;;
;;; Return Value
;;; integer
;;;
;;; Example
#|
(get-dimensions (event :location '(0 1 0))) --> 3
|#
;;;
;;; Last Modified
;;; 2020/06/25
;;;
;;; Synopsis
(defmethod get-dimensions ((obj event))
;;; ****
  (let ((dim (max (length (flat (location obj)))
		  (length (flat (expansion obj))))))
    (loop for child in (events obj) do
	 (let ((c-dim (get-dimensions child)))
	   (when (< dim c-dim)
	     (setq dim c-dim))))
    dim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* event/set-dimensions
;;; Name
;;; set-dimensions
;;;
;;; File
;;; event.lsp
;;;
;;; Description
;;; Sets the amount of dimensions of an event and all subevents
;;; (= cuts the length of lists in location and expansion
;;; or fills it with 0s).
;;;
;;; Arguments
;;; obj: a placed-object
;;;
;;; Return Value
;;; integer
;;;
;;; Example
#|
(set-dimensions (event :location '(0 1 0)) 2) --> location: '(0 1)
(set-dimensions (event :location '(0 1 0)) 4) --> location: '(0 1 0 0)
|#
;;;
;;; Last Modified
;;; 2020/06/22
;;;
;;; Synopsis
(defmethod set-dimensions ((obj event) dimensions)
;;; ****
  (doevents (e obj)
    (call-next-method e dimensions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF event.lsp
