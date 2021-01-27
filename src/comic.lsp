;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; event/comic.lsp                                                           2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* event/comic
;;; Name
;;; comic
;;;
;;; File
;;; comic.lisp
;;;
;;; Description
;;; Definition of comic-class.
;;;
;;; Slots
;;; Direct slots: title, subtitle, author, date
;;; ...as well as all slots of the event and named-object classes
;;;
;;; Last Modified
;;; 2019/12/21
;;;
;;; Synopsis
(defclass comic (event)
  ((title :accessor title
	  :initarg :title
	  :initform "untitled")
   (subtitle :accessor subtitle
	     :initarg :subtitle
	     :initform nil)
   (author :accessor author
	     :initarg :author
	     :initform nil)
   (date :accessor date
	 :initarg :date
	 :initform (todays-date))
   (protagonists :initarg :protagonists
		 :initform nil)))
;;; ****
(cc-make-accessor protagonists comic '(list protagonist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/comicp
;;; Name
;;; comicp
;;;
;;; File
;;; comic.lisp
;;;
;;; Description
;;; returns t if the specified object is a comic
;;;
;;; Arguments
;;; obj: any object
;;;
;;; Return Value
;;; boolean
;;;
;;; Example
;;; (comicp my-comic) --> t
;;;
;;; Last Modified
;;; 2019/12/21
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comicp (obj)
  "Checks if an object is a comic"
  (equal 'comic (type-of obj)))

(defmethod print-object :before ((obj comic) stream)
  (format stream "#<COMIC: ~a" (name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-slots append ((obj comic))
  '(title subtitle author date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-clone-duplicate-events (events)
  ;; If a comic is made using the same instance of an event multiple
  ;; times, all duplicates must be cloned before assigning IDs! This
  ;; way, later edits will only affect the events intended. Once a
  ;; comic is intantialised using make-comic, each event will be
  ;; assigned an individual ID and an error will be signaled, if an ID
  ;; occurs twice.
  (let ((seen nil))
    (labels ((helper (events)
	       (if (listp events)
		   ;; when list, call for each element
		   (loop for e in events
			 collect
			 (helper e))
		   ;; if event: collect event or clone...
		   (prog1
		       (if (member events seen)
			   (clone events)
			   events)
		     ;; remember which events we've seen...
		     (push events seen)
		     ;; if event has subevents, treat them just
		     ;; the same: 
		     (when (events events)
		       (setf (slot-value
			      events 'events)
			      (helper (flat (events events)))))))))
      ;; Call helper to start iteration. Remember: The new comic, event
      ;; or event list will be returned, not set in the same place as
      ;; the old one (non-destructive)!
      (if (listp events)
	  (helper (flat events))
	  (helper events)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-assign-ids-to-events (events)
  (ensure-list events)
  (let ((id-count 0)
	(assigned nil))
    (labels (;; return all ids that are already
	     ;; set in a list
	     (get-ids (ls)
	       (when (eventp ls)
		 (setq ls (list ls)))
	       (loop for event in ls
		     for n from 0 do
		       (let ((id (id event)))
			 (when id
			   (if (member id assigned)
			       (cc-error 'assigning-IDs-to-events
				   "The ID ~d is assigned to ~
                                 multiple events."
				 id)
			       (push (id event) assigned)))
			 (when (events event)
			   (get-ids (events event))))))
	     ;; find an id that is not yet assigned
	     (find-id ()
	       (when (member id-count assigned)
		 (incf id-count)
		 (find-id)))
	     ;; set the id of all events in ls and
	     ;; their child-events
	     (set-ids (ls)
	       (when (eventp ls)
		 (setq ls (list ls)))
	       (loop for event in ls do
		 (unless (id event)
		   (find-id)
		   (id event id-count)
		   (incf id-count))
		 (when (events event)
		   (set-ids (events event))))))
      (get-ids events)
      (set-ids events)
      events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; check if all objects passed as events are actually events:
(defun check-all-events (comic &optional (context 'PREPARE))
  (labels ((helper (e)
	     (if (eventp e)
		 (loop for child in (flat (events e))
		       do
			  (if (comicp e)
			      (cc-error context
				  "Comic found in event-slot.~%~
                               A comic must always be the outermost ~
                               object of a structure!~%~
                               event: ~a~%comic: ~a" e child)
			      ;; if event but no comic: success!
			      ;; continue: 
			      (helper child)))
		 (cc-error context
		     "Illegal object \"~a\" found in an event-slot.~%~
                      Check your data!"
		   e))))
    (loop for e in (flat comic) do
      (helper e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-can-render-times (comic &optional (context 'PREPARE))
  (doevents (e comic)
    (let* ((min-dur (get-min-dur e))
	   (max-dur (get-max-dur comic (id e))))
      (when (and min-dur max-dur)
	(let 
	    ;; Lisp will sometimes produce some rounding-errors –
	    ;; let's be a little generous here...:
	    ((maaax-dur (* 1/100 (ceiling (* 100 max-dur)))))
	  (when (u> min-dur maaax-dur)
	    (cc-error context
		"The event with ID ~d, duration ~a and ~
               start-time ~a~%exceeds its maximum ~
               duration of ~a."
	      (id e) (duration e) (start-time e) (secs max-dur))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/make-comic
;;; Name
;;; make-comic
;;;
;;; File
;;; comic.lisp
;;;
;;; Description
;;; Creates a comic and checks if given values are of approprate
;;; types. If a name is supplied, the comic will be bound to that
;;; symbol and be available as a global variable.
;;;
;;; The function also checks the start-time and duration-slots
;;; of all given events and subevents (and so on) for consistency:
;;; E.g. if the comic has a duration of 1 minute, but a given event
;;; starts at minute 2, an error will be signaled.
;;;
;;; A comic is an event and inherits all slots from the event class.
;;; Additional slots of the comic class, like title, subtitle, author
;;; and date have no special purpose per se, but may be accessed by
;;; render-modes, e.g. to write meta-data to a file, a header to a
;;; document, etc.
;;;
;;; Arguments
;;; name (symbol)
;;;
;;; Optional Arguments
;;;
;;; title (string or symbol, default: "untitlted")
;;; subtitle (string or symbol)
;;; author (string or symbol)
;;; date (string or symbol, default: todays date)
;;; ... as well as all slots of the event class.
;;;
;;; Return Value
;;; comic
;;;
;;; Example
;;; (make-comic '+my-comic+ :events (...))
;;;
;;; Last Modified
;;; 2020/06/20
;;;
;;; Synopsis
(defun make-comic
    (name ; if a name is supplied, it will be a global variable
     &rest
       args
     &key
       (title "untitled")
       subtitle
       author
       (date (todays-date))
       events
       &allow-other-keys)
;;; ****
  ;;an empty variable to be filled with arguments for make-instance
  (let ((set-slots))
    ;;type-check direct comic-slots (and name) and add to the list
    (loop
      for arg in
      '(name title subtitle author date)
      for val in
	      (list name title subtitle author date)
      do
	 (unless (type-or val '(string symbol null))
	   (cc-error 'MAKE-COMIC
	       "Wrong type of argument for ~a:~%~
               Should be a symbol or a string, ~
               not ~a." arg (type-of val)))
	 (push val set-slots)
	 (push (make-keyword arg) set-slots))
    ;; type-check event-slots and add to the list
    (loop for arg in (cc-get :event-slots)
	  for val = (getf args (make-keyword (car arg)))
	  do
	     (when val
	       (if (type-or val (cdr arg))
		   (progn
		     (push val set-slots)
		     (push (make-keyword (car arg)) set-slots))
		   (cc-error 'MAKE-COMIC
		       "Wrong type of argument for ~a~%~
                    Should be of type ~a, not ~a."
		     (car arg) (cons 'or (cdr arg)) (type-of val)))))
    ;; make sure all give values passed as events are indeed events:
    (check-all-events events 'MAKE-COMIC)
    ;; make the instance, if name is set it will be global,
    ;; assign ids to events and return the object return it:
    (let ((the-comic (apply #'make-instance 'comic set-slots)))
      ;; clone all duplicate events to assign unique IDs:
      (setq the-comic (cc-clone-duplicate-events the-comic))
      ;; assign IDs:
      (cc-assign-ids-to-events the-comic)
      (when name
	(eval `(defparameter ,name ,the-comic)))
      ;; make sure all given durations and start-times are ok
      (check-can-render-times the-comic 'MAKE-COMIC)
      the-comic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* comic/get-event-by-id
;;; Name
;;; get-event-by-id
;;;
;;; File
;;; comic.lsp
;;;
;;; Description
;;; Returns the event with the given ID from a comic.
;;;
;;; Parameter
;;; comic: the comic
;;; id: the id
;;;
;;; Return Value
;;; event
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; Synopsis
(defmethod get-event-by-id ((comic comic) id)
;;; ****
  (doevents (e comic)
    (when (eq (id e) id)
      (return-from get-event-by-id e)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/add-events
;;; Name
;;; add-events
;;;
;;; File
;;; comic.lsp
;;;
;;; Description
;;; Adds events to a comic. If an id is supplied, the events will
;;; be added to the event with that id. Otherwise the events will
;;; be added directly to the events-slot of the comic.
;;;
;;; When called with a comic, the method will also assign ids to 
;;; the new events.
;;;
;;; Parameter
;;; comic: the comic to modify
;;; events: the events to add (Can be a list of events or a single
;;; event.)
;;; id: If supplied, the id of the event to add the events to.
;;;
;;; Return Value
;;; boolean (t if events were added, else nil)
;;;
;;; Example
#|
(add-events my-comic (make-event)) --> t
(add-events my-comic (make-event) 10) 
--> t if event with id 10 was found, else nil
|#
;;;
;;; Last Modified
;;; 2020/02/01
;;;
;;; Synopsis
(defmethod add-events ((comic comic) events &optional id)
;;; ****
  (if id
      (let ((e (get-event-by-id comic id)))
	(if e
	    (add-events e events)
	    (return-from add-events nil)))
      ;; Call the event-method to actually add the events
      (call-next-method)))

(defmethod add-events :after ((comic comic) events &optional id)
  (declare (ignore id))
  (cc-assign-ids-to-events comic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/delete-event
;;; Name
;;; delete-event
;;;
;;; File
;;; comic.lsp
;;;
;;; Description
;;; Deletes event with given ID from comic, including all subevents.
;;;
;;; Arguments
;;; comic: the comic
;;; id: the id of the event to delete
;;;
;;; Optional Arguments
;;; print: print to std-out when event is deleted
;;;
;;; Return Value
;;; boolean: t if event deleted, else nil
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; Synopsis
(defmethod delete-event ((comic comic) id &optional (print t))
;;; ****
  (let ((success nil))
    (labels ((helper (events)
	       (let ((e (car events)))
		 (unless (null e)
		   (if (= id (id e))
		       (progn
			 (setq success t)
			 (when print
			   (format
			    t "~&Deleting event with ID ~a. ~
                           It has ~d subevents."
			    (id e) (count-events e)))
			 (helper (cdr events)))
		       (progn
			 (events e (helper (events e)))
			 (cons e (helper (cdr events)))))))))
      (events comic (helper (events comic)))
      success)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/get-events-by-time
;;; Name
;;; get-events-by-time
;;;
;;; File
;;; comic.lsp
;;;
;;; Description
;;; Returns all events active at a given point of time.
;;; Be carefull to (prepare comic) first, as the function 
;;; only looks at the values of a single event.
;;;
;;; Arguments
;;; comic: The comic. Can also be an event, but should idealy
;;;        be a prepared comic. 
;;; time: The point in time. Value defaults to seconds.
;;;
;;; Return Value
;;; event
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; Synopsis
(defun get-events-by-time (comic time)
;;; ****
  "returns all events at a certain point in time"
  (setq time (secs time))
  (labels ((in-time? (e)
	     (let* ((st (secs (start-time e)))
		    (dur (secs (duration e))))
	       (cond ((and st dur)
		      (when (and (u>= time st)
				 (u<= time (u+ st dur)))
			t))
		     (st (when (u>= time st) t))))))
    (if (in-time? comic)
	(let ((ls nil))
	  (doevents (e comic)
	    (when (in-time? e) (push e ls)))
	  (reverse ls))
	(cc-error 'GET-EVENTS-BY-TIME
	    "Invalid time for a comic of duration ~d"
	  (duration comic)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/get-event-by-name
;;; Name
;;; get-event-by-name
;;;
;;; File
;;; comic.lsp
;;;
;;; Description
;;; Returns all events with a given name from a comic (or event).
;;;
;;; Arguments
;;; comic: the comic. Can also be an event.
;;; a name (symbol)
;;;
;;; Return Value
;;; event or list (if multiple results)
;;;
;;; Last Modified
;;; 2020/02/16
;;;
;;; Synopsis
(defun get-event-by-name (comic name)
;;; ****
  "returns all events at a certain point in time"
  (let ((result nil))
    (doevents (e comic)
      (when (eq (name e) name)
	(push e result)))
    (cond ((= 0 (length result))
	   nil)
	  ((= 1 (length result))
	   (first result))
	  (t
	   (reverse result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* comic/check-comic
;;; Name
;;; check-comic
;;;
;;; File
;;; comic.lisp
;;;
;;; Description
;;; Checks the state of a comic-object:
;;; -> Do all events have an ID?
;;; -> Are the start-time and duration values consistent?
;;; 
;;; Arguments
;;; comic: the comic-object
;;;
;;; Optional Arguments
;;; print (default t): Print detailed information to stream
;;;
;;; Return Value
;;; boolean: If the comic object passes all test T, else NIL
;;;
;;; Example
#|
(check-comic +test-comic+) --> NIL, because it holds added events
                                    without IDs.
(progn 
  (cc-prepare +test-comic)
  (check-comic +test-comic+)) --> T
|#
;;;
;;; Last Modified
;;; 2020/02/06
;;;
;;; Synopsis
(defun check-comic (comic &optional (print t))
;;; ****
  "Returns t if the comic is properly created and can be rendered.
   Also prints out information on the state of the comic."
  ;; hard test: is the comic renderable at all?
  ;; if check unsuccessfull, error
  (check-can-render-times comic 'CHECK-COMIC)
  ;; soft tests: are any information missing?
  ;; if check unsuccessfull, print warning to std-out
  (let* ((all-ids-set
	  (doevents (e comic) (unless (id e) (return nil)))))
    (when print
      (format print "All IDs set: ~a" all-ids-set))
    (and all-ids-set)))
;; CHECK IF ALL SLOTS HOLD STRINGS, PROPERTIES OR LISTS!
;; NUMBERS SHOULD ALL BE FILLED BY CC-AUTOSET-NUMERIC-VALUES

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF comic/comic.lsp
