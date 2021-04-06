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
;; render/prepare-render.lsp                                                 2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; wenn man eine start-time aber keine duration angiebt: wie lang ist dann das event
;; wenn keine min-dur??

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun is-parent? (event)
  (when (events event) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun secs-> (event)
  "convert all time-values to seconds as plain numbers"
  (cond ((eventp event)
	 (doevents (e event)
	   (when (duration e)
	     (duration e (secs-> (duration e))))
	   (when (start-time e)
	     (start-time e (secs-> (start-time e))))))
	((null event)
	 nil)
	(t (value (convert event 'secs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ->secs (event)
  "convert all time-values to seconds as unit-objects"
  (cond ((eventp event)
	 (doevents (e event)
	   (when (duration e)
	     (duration e (->secs (duration e))))
	   (when (start-time e)
	     (start-time e (->secs (start-time e))))))
	((null event)
	 nil)
	(t (convert event 'secs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-parent-list (comic id)
  "returns a list of a events that represents the inheritance of
   an event in the form of '(parent grand-parent...)"
  (if (= id 0); 0 is the comic-id and a comic has no parents
      nil
      (let ((ls (list comic)))
	(labels ((helper (ls)
		   (let ((children (events (car ls))))
		     (when children
		       (loop for child in (flat children) do
			 (if (eq id (id child))
			     (return-from get-parent-list ls)
			     (helper (cons child ls))))))))
	  (helper ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-siblings-list (comic id)
  "returns a list of a events that are siblings of an event
   in a comic"
  (let ((parent (car (get-parent-list comic id)))
	(siblings nil))
    (when parent
      (loop for sib in (flat (events parent)) do
	   (unless (eq (id sib) id)
	     (push sib siblings)))
      siblings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/get-min-dur
;;; Name
;;; get-min-dur
;;;
;;; File
;;; prepare-render.lsp
;;;
;;; Description
;;; Recursively determines the minimum duration an event must have
;;; depending on its subevents.
;;;
;;; Parameter
;;; event: the event
;;;
;;; Return Value
;;; number (in secs)
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-min-dur (event)
  "go through all children recursively and
   find the minimal duration event must have"
  (let ((all-durs))
    (labels ((helper (event offset)
	       (let ((dur (duration event))
		     (evs (events event)))
		 (if evs
		     (loop for child in (flat evs)
			collect
			  (let ((st-c (start-time child)))
			    (helper child
				    (u+ offset
					(if st-c st-c 0)))))
		     (push (secs-> (if dur
				       (u+ offset dur)
				       offset))
			   all-durs)))))
      (helper event 0)
      (apply #'max all-durs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* event/get-max-dur
;;; Name
;;; get-max-dur
;;;
;;; File
;;; prepare-render.lsp
;;;
;;; Description
;;; Recursively determines the maximum duration an event can have
;;; depending on its position in a comic structure.
;;;
;;; Parameter
;;; event: the event
;;;
;;; Return Value
;;; number (in secs)
;;;
;;; Last Modified
;;; 2020/02/09
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-max-dur (comic id)
  "go through all events recursively and
   find the maximal duration event can have"
  (let* ((event (get-event-by-id comic id))
	 (parents
	  (get-parent-list comic id))
	 (shortest-parent nil)
	 (parent-position (if (start-time event)
			      (secs-> (start-time event))
			      0)))
    (when parents
      (loop for parent in (flat parents)
	 do
   	   (let ((parent-dur (secs-> (duration parent)))
		 (parent-st (secs-> (start-time parent))))
	     (when parent-dur
	       (decf parent-dur parent-position)
	       (if shortest-parent
		   (when (u< parent-dur shortest-parent)
		     (setq shortest-parent parent-dur))
		   (setq shortest-parent parent-dur)))
	     (when parent-st
	       (incf parent-position parent-st)))))
    shortest-parent))

(defun cc-auto-fill-coordinates (events)
  "Fill coordinates. All values default to 0"
  ;; ensure all slots are lists
  (doevents (e events)
    (location e (flat (location e)))
    (expansion e (flat (expansion e))))
  (let ((dimensions 1))
    ;; find the maximum dimension first
    (doevents (e events)
      (let ((dim (max (length (location e))
		      (length (expansion e)))))
	(when (> dim dimensions)
	  (setq dimensions dim))))
    ;; fill all coordinates with 0's if too short
    (doevents (e events)
      (let ((loc (location e))
	    (expan (expansion e)))
	(location e (append loc
			    (set-length
			     (- dimensions (length loc))
			     0)))
	(expansion e (append expan
			     (set-length
			      (- dimensions (length expan))
			      0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inherit properties from parent to child event.
;; Inherits every unit except id, events, duration, start-time
;; Deletes the inherited unit in the parent event. It will only
;; serve as a container for placing children in time!
;; When :delete-parent-data is set to NIL, this process will be
;; skipped, causing parent-events to be rendered as well.
(defun cc-inherit-properties (event &key (delete-parent-data t))
  (let ((properties)
	(children))
     (loop for prop in (cc-get :event-slots)
	do
	  (unless (member (car prop) '(name id events
				       start-time duration))
	    (push (car prop) properties)))
     (doevents (parent event)
       (setq children (events parent))
       (when children
	 (loop for prop in properties
	    do
	      (let ((parent-val (funcall prop parent)))
		(when parent-val
		  (loop for child in (flat children)
		     do
		       (unless (funcall prop child)
			 (funcall prop child parent-val)))
		  (when delete-parent-data
		    (funcall prop parent nil)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sort-by-start-time (events)
  "Sorts events by start-time. Non-recursive!"
  (when events
    (sort events
	  (lambda (ev1 ev2)
	    (when (u< (start-time ev1)
		      (start-time ev2))
	      t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-times (comic)
  "Check if all start-time and duration slots are set."
  (doevents (e comic)
	    (unless (duration e)
	      (return nil))
	    (unless (or (start-time e)
			(= (id e) 0))
	      (return nil))
	    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-no-times-set? (comic)
  "Returns t if all durations and start-time slots of a comic are
     either nil or equal to 0"
  (let ((ret t))
    (doevents (e comic)
      (let ((st (start-time e)) (dur (duration e)))
	(when st (unless (u-zerop st) (setq ret nil)))
	(when dur (unless (u-zerop dur) (setq ret nil)))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-return-first-baby (comic)
  "Returns the first event in a comic with event-slot = nil"
  (if (events comic)
      (cc-return-first-baby (car (flat (events comic))))
      comic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-set-parent-duration (comic id &optional print)
  "Sets the duration of an event (must be a parent) by looking at its
   children."
  (let ((e (get-event-by-id comic id)))
    (unless (duration e)
      (let* ((min-dur (get-min-dur e))
	     (max-dur (get-max-dur comic (id e)))
	     (new-val nil))
	(cond
	  ;; if we have a max dur, choose max-dur.
	  (max-dur
	   (setq new-val max-dur))
	  ;; if there is no max dur and min dur is <= 0, set
	  ;; the duration according to siblings average duration
	  ((<= min-dur 0)
	   (let* ((sib-durs))
	     (loop for sib in (flat (get-siblings-list comic id))
		   do
		      (when (duration sib)
			(push (duration sib) sib-durs)))
	     (when (> (length sib-durs) 0)
	       (setq new-val (u/ (apply #'u+ sib-durs)
				 (length sib-durs))))))
	  ;; if min dur is > 0 and there is no max-dur,
	  ;; set the new value to either to min-dur or to the
	  ;; shortest dur multiplied with siblings amount
	  (t (setq new-val
		   (max min-dur
			(loop for c in (events e)
			      for dc = (value (duration c) 'secs)
			      when dc
				minimize dc into r
			      finally (return (if r (* r (length
							  (events
							   e)))
						  0)))))))
	(when new-val
	  (when print
	    (format
	     t "~&Duration of event with ID ~d set to ~d"
	     id (->secs new-val)))
	  (duration e new-val))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-auto-complete-start-time (parent &optional print)
  (let* ((parent-dur (duration parent))
	 (children (flat (events parent)))
	 (next-child-n (length children))
	 (next-st parent-dur))
    ;; The first childs start-time defaults
    ;; to 0
    (unless (start-time (car children))
      (start-time (car children) 0)
      (when print 
	(format
	 t "~&Start-time of event with ID ~d set to ~d"
	 (id (car children)) (->secs 0))))
    ;; start looping
    (loop for child in children
	  for next-child in (cdr children)
	  for st = (start-time child)
	  for dur = (duration child)
	  for n from 0
	  do
	     (unless (start-time next-child)
	       ;; find bounds for interpolation loop
	       (loop for c in (nthcdr (1+ n) children)
		     for m from (1+ n)
		     do
			(when (start-time c)
			  (setq next-st (start-time c)
				next-child-n m)
			  (return))
		     finally
			;; if no next-st found,
			;; it defaults to end of parent
			(setq next-st parent-dur
			      next-child-n (1+ m)))
	       ;; make interpolation
	       (let* ((amount (- next-child-n n)))
		 (unless (<= amount 0)
		   (when st
		     (loop for c in (nthcdr n children)
			   for i below amount
			   for new-st
			     in (line (secs-> st)
				      (list (secs-> next-st)
					    amount))
			   do
			      ;; when event is too long, reset new start-time
			      ;; accordingly:
			      (when
				  (and (duration c)
				       (u> (u+ new-st (duration c)) parent-dur))
				(setq new-st (u- parent-dur (duration c))))
			      ;; set the value (the unless-statement should not be
			      ;; necassary, just to be 100% sure the value is not set
			      (unless (start-time c)
				(when print 
				  (format
				   t "~&Start-time of event with ID ~d set to ~d"
				   (id c) (->secs new-st)))
				(start-time c new-st))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-auto-complete-durations (parent &optional print)
  "when all start-times are set, we can
   sort them and set durations"
  (let ((events (events parent)))
    (when events
      ;;    if there is only one child:
      (if (= 1 (length events))
	  (let* ((event (car events))
		 (dur (duration event))
		 (st (start-time event))
		 (parent-dur (duration parent)))
	    (when (and (not dur) st parent-dur)
	      (duration event (u- parent-dur st))))
	  ;; if there is more than on child:
	  (progn
	    (setq events (flat events))
	    (when (all? #'start-time events)
	      (setq events
		    (sort-by-start-time events))
	      (let ((next-sts
		     (append
		      (mapcar #'start-time (cdr events))
		      (list (duration parent)))))
		(loop for e in events
		   for n from 0
		   do
		     (let ((dur (duration e))
			   (st (start-time e))
			   (min-dur (get-min-dur e)))
		       (unless min-dur (setq min-dur 0))
		       (when (and (not dur) st)
			 (loop for next-st in (nthcdr n next-sts)
			    do
			      (when next-st
				(let ((dur (u- next-st st)))
				  (when (and (u> dur 0)
					     (u> dur min-dur))
				    (duration e (secs dur))
				    (when print
				      (format
				       t "~&Duration of event with ID ~d set to ~d"
				       (id e) (secs dur))))))
			      (return))))
		   finally (return t)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-auto-complete-time (comic &optional print)
  (let* ((parent-lists))
    ;; are all values plausible?
    (check-can-render-times comic)
    ;; convert all unit-objects to plain numbers in seconds:
    (secs-> comic)
    ;; in case we have no values at all, set the first "babys" (event
    ;; without children) duration to 1 second. Just a choice.
    (when (cc-no-times-set? comic)
      (duration (cc-return-first-baby comic) 1))
    (doevents (e comic)
      ;; simple auto-completion of durations in case
      ;; all start-times are already set
      (cc-auto-complete-durations e print)
      ;; push parents of last-children into parent-list
      (unless (is-parent? e)
	(let ((parents (get-parent-list comic (id e))))
	  (when parents
	    (push parents parent-lists)))))
    ;; sort parent-lists by length to find the amount of generations
    (setq parent-lists
	  (sort parent-lists (lambda (ls1 ls2)
			       (when (> (length ls1) (length ls2)) t))))
    (loop for generation from (length (car parent-lists)) downto 0
       do
	 (loop for ls in parent-lists
	    do
	      (let ((e (nth generation (reverse ls))))
		(when e
		  ;; set e's duration by minimum and
		  ;; maximum values and average...
		  (unless (duration e)
		    (cc-set-parent-duration comic (id e) print))
		  ;; ...auto-complete its children
		  (when (and (is-parent? e)
			     (duration e))
		    ;; ...first, the start-times
		    (cc-auto-complete-start-time e print)
		    ;; ...second, the durations
		    (cc-auto-complete-durations e print))))))
    ;; finally loop through all events again
    (doevents (e comic)
      ;; set durations of all events...
      (cc-set-parent-duration comic (id e) print)
      ;; ...and all remaining start-times to 0
      (unless (start-time e)
	(start-time e 0)))
    ;; (get-longest-child-dur comic);CHECK
    (->secs comic)
    (check-times comic)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-tempo-deviation (bpm events
			    &key
			      (metric-layers 4)
			      (time-position 0))
  "Returns a deviation-factor for a list of events and a grid of beats
   at a given tempo. If all times are on the beat, deviation is 0"
  ;; calculate a single time-values deviation from metric grid:
  (labels ((get-dev (val bpm c)
	     (multiple-value-bind (beat dev)
		 (round (/ val (/ 1 (/ bpm 60))))
	       (declare (ignore beat))
	       (let ((the-dev (abs dev)))
		 (if (> c 0)
		     (+ the-dev
			(get-dev the-dev (* 2 bpm) (1- c)))
		     0)))))
    (let ((times
	    ;; all start- and end-times of events in seconds:
	    (loop for e in events
		  append
		  (let* ((st (value e 'secs 'start-time))
			 (dur (value e 'secs 'duration)))
		    (list (- st time-position)
			  (- (+ st dur) time-position))))))
      (loop for time in times
	    summing
	    (get-dev time bpm metric-layers)
	      into result
	    finally (return (/ result (length times)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-tempo (events
		   &key
		     (metric-layers 4)
		     (time-position 0)
		     print)
  "Detect a good tempo for events in bpm. Return success-factor (0-1)
   as second value."
  (apply
   #'values
   (car
    (sort 
     (loop for tempo from 60 to 120
	   collect
	   (let ((dev (get-tempo-deviation
		       tempo events
		       :metric-layers metric-layers
		       :time-position time-position)))
	     (when print
	       (format t "~&~a (~a)" tempo (float dev)))
	     (list tempo dev)))
     #'< :key #'second))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-flatten-events (comic)
  "Flattens the event list of a comic. All parents should only have
   values for duration and start-time: Run auto-completion first!"
  (doevents (e comic)
    (let* ((st-e (start-time e))
	   (id-e (id e))
	   (children (events e))
	   (p (car (get-parent-list comic id-e))))
      ;; set start-time to start-time child + start-time parent
      (when (and children st-e p)
	(loop for c in (flat children) do
	  (let ((st-c (start-time c)))
	    (when st-c
	      (start-time c (u+ st-c st-e)))))
	;; add the children to the new parent
	(events p (loop for ev in (flat (events p))
			append
			(if (eq ev e)
			    children
			    (list ev))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric prepare (obj &optional
			   print
			   auto-complete-time
			   auto-complete-space))

(defmethod prepare ((comic event)
		    &optional
		      print
		      (auto-complete-time t)
		      (auto-complete-space t))
  (format t "~&~%Preparing ~a" comic)
  (doevents (e comic)
    (events e (flat (events e))));flatten event-list
  ;; Assign IDs
  (cc-assign-ids-to-events comic)
  ;; (format t "~&Setting numeric values to default units...")
  ;; (cc-auto-set-numeric-values comic)
  ;; Inheriting properties to subevents
  (cc-inherit-properties comic)
  (when auto-complete-time
    (when print 
      (format t "~&Auto-completing start-times and durations..."))
    (cc-auto-complete-time comic print))
  ;;Flattening event-list...
  (cc-flatten-events comic)
  (when auto-complete-space
    (when print
      (format t "~&Auto-completing location and expansion..."))
    (cc-auto-fill-coordinates comic)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF prepare-render.lsp
