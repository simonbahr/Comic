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
;; analyse/analyse.lsp                                                       2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;; TODO
;; -> Add a :round option to analyse, which will round numeric values
;;    to e.g. 0.01, 0.02... Would decrease amount of data in analysis
;;    file and make more sense also. Must also be implemented in
;;    reverse algorithms!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ANALYSE                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Not used in Analysis-Suite so far. May be useful in the future...
(defun cc-get-event-creation-args (event &optional (return-nil-values nil))
"returns the key-arguments of an event in a list. Non-recursive!"
(let ((creation-args nil))
  (loop for key in (cc-get :event-slots) do
       (let* ((slotname (car key))
	      (arg (make-keyword slotname))
	      (val (funcall slotname event)))
	 (when (or return-nil-values val)
	   (when (eq arg :events)
	     (setq val (loop for e in (flat val) collect
			    (cc-get-event-creation-args e))))
	   (push val creation-args) (push arg creation-args))))
  creation-args))

;;; Returns the creation form for a unit-object.
;;; #<hz 100> --> (hz 100)
(defun cca-get-unit-value-ls (unit-obj)
  (declare (type unit unit-obj))
  (let ((val (value unit-obj)))
    (list (type-of unit-obj)
	  (if (symbolp val)
	      `(quote ,val)
	      val))))

;;; Returns a readable object that can be printed into
;;; the analysis-file and later be read back in.
(defun cca-get-readable-obj (obj)
  (if (unit-p obj)
      (cca-get-unit-value-ls obj)
      obj))

;;; returns a cons-cell containing a datum as car
;;; and a counter as cdr --> ((secs 1) . 1)
(defun cca-make-data-count (data &optional (count 1))
  (cons data count))

;;; increases the cdr of a data-count-cell by 1
;;; --> ((secs 1) . 2)
(defun cca-inc-data-count (data-count)
  (cons (car data-count) (1+ (cdr data-count))))

;;; adds a datum to a list of data,
;;; either by consing it to the data-list
;;; if not found or by increasing its count
;;; if already found in the data-list
(defun cca-add-obj-to-data-ls (data-ls obj)
  (setq obj (cca-get-readable-obj obj))
  (let* ((found-entry nil)
	 (result
	  (cons (car data-ls)
		(loop for data-count in (cdr data-ls) collect
		     (if (equal (car data-count) obj)
			 (progn
			   (setq found-entry t)
			   (cca-inc-data-count data-count))
			 data-count)))))
    (unless found-entry
      (setq result (cons (car result)
			 (cons (cca-make-data-count obj)
			       (cdr result)))))
    result))

;;; makes an object with a key, like:
;;; (DURATION (secs 1))
(defun cca-make-key-obj (slot-name value)
  (list slot-name (cca-get-readable-obj value)))

;;; makes a countable object with a key, like:
;;; (DURATION ((secs 1) .1))
(defun cca-make-key-obj-count (key-obj &optional (count 1))
  (list (car key-obj) (cons (cadr key-obj) count)))

;;; adds an object with a key to a list of other
;;; objects with keys, either by consing or by
;;; increasing the count
(defun cca-add-obj-by-key (key-ls key-obj)
  (let* ((found-entry nil)
	 (result
	  (loop for data-ls in key-ls collect
	       (if (equal (car data-ls) (car key-obj))
		   (progn
		     (setq found-entry t)
		     (cca-add-obj-to-data-ls data-ls (cadr key-obj)))
		   data-ls))))
    (unless found-entry
      (push (cca-make-key-obj-count key-obj) result))
    result))

;;; adds a new entry (= an object with a key associated with a list
;;; of countable objects with keys, initially nil)
(defun cca-add-entry (analysis-data key-obj)
  (cons (cons key-obj nil) analysis-data))

;;; associates a countable object with key (key-obj2) with
;;; an entry lead by key-obj1 in analysis-data
(defun cca-associate-objects (analysis-data key-obj1 key-obj2)
  (let* ((found-entry nil)
	 (result
	  (loop for entry in analysis-data collect
	       (if (equal key-obj1 (car entry))
		   (progn
		     (setq found-entry t)
		     (cons (car entry)
			   (cca-add-obj-by-key (cdr entry) key-obj2)))
		   entry))))
    (if found-entry
	result
	(cca-associate-objects (cca-add-entry analysis-data key-obj1)
			       key-obj1 key-obj2))))

;;; analyse a list of (possibly nested) events and write
;;; the analysis to a file. It looks e.g. like this:
;; (((DURATION (SECS 4.6671877)) ; <-- key of entry
;;   (AMPLITUDE ((MIDIVELOCITY 67) . 1) ((MIDIVELOCITY 83) . 1))
;;   (PITCH ((MIDINOTE 29) . 2)))
;;  ((DURATION (SECS 0.1984375)) ; <-- second entry
;;   (AMPLITUDE ((MIDIVELOCITY 75) . 2) ((MIDIVELOCITY 87) . 2))
;;   (PITCH ((MIDINOTE 55) . 2) ((MIDINOTE 56) . 2)))
;;  ((DURATION (SECS 0.596875))  ; <-- third entry
;;   (AMPLITUDE ((MIDIVELOCITY 90) . 1) ((MIDIVELOCITY 102) . 1))
;;   (PITCH ((MIDINOTE 36) . 2)))
;; ...)

(defun analyse (events analysis-file
		&key
		  (exclude-slots '(id events))
		  (use-tempered-scale t)
		  (use-dynamic-symbols t)
		  (round-time-to-msecs t))
  (let* ((analysis-data
	  (when (probe-file analysis-file)
	    (with-open-file (file analysis-file
				  :direction :input)
	      (read file)))))
    ;; a little helper for preparing data according to
    ;; specified key-args
    (flet ((prepare-data-for-analysis (slot-name val)
	     (cond
	       ;; for events: save amount of direct subevents
	       ;; (not used yet, but could be useful)
	       ((eq slot-name 'events) (length (flat val)))
	       ;; use rounded midinote-numbers for pitches(ana
	       ;; if use-tempered-scale is t
	       ((and use-tempered-scale (eq slot-name 'pitch))
		(midinote
		 (round (value (hz val) 'midinote))))
	       ;; "round" amplitudes by converting to dynamic-symbols
	       ;; if use-dynamic-symbols is t
	       ((and use-dynamic-symbols (eq slot-name 'amplitude))
		(dynamic-symbol (amp val)))
	     ;; round start-time and duration to milliseconds
	     ;; if round-time-to-msecs is t
	       ((and round-time-to-msecs
		     (member slot-name '(start-time duration)))
		(msecs (* 10 (round (value (secs val) 'msecs) 10))))
	       (t val))))
      ;; start to analyse events
      (doevents (event events)
	(loop for key-slot in (cc-get :event-slots) do
	     (let* ((key-name (car key-slot))
		    (key-val (funcall key-name event)))
	       (when key-val
		 (setq key-val (prepare-data-for-analysis key-name key-val))
		 (unless
		     (member key-name (cons 'events exclude-slots))
		   (loop for slot in (cc-get :event-slots) do
			(let* ((slot-name (car slot))
			       (slot-val (funcall slot-name event)))
			  (when slot-val
			    (unless
				(member slot-name
					(cons key-name exclude-slots))
			      (setq slot-val
				    (prepare-data-for-analysis slot-name
							       slot-val))
			      (setq analysis-data
				    (cca-associate-objects
				     analysis-data
				     (cca-make-key-obj
				      key-name key-val)
				     (cca-make-key-obj
				      slot-name slot-val))))))))))))
      (with-open-file (file analysis-file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
	(print analysis-data file))))
  t)

;;; analyse a midi file directly into an analysis file
(defun analyse-midi-files (midi-files analysis-file
			   &key
			     analyse-directory
			     (exclude-slots '(id events))
			     (use-tempered-scale t)
			     (use-dynamic-symbols t)
			     (round-time-to-msecs t))
  (cond ((listp midi-files)
	 (loop for mf in midi-files do
	      (analyse-midi-files
	       mf analysis-file
	       :analyse-directory analyse-directory
	       :exclude-slots exclude-slots
	       :use-tempered-scale use-tempered-scale
	       :use-dynamic-symbols use-dynamic-symbols
	       :round-time-to-msecs round-time-to-msecs)))
	(analyse-directory
	 (analyse-midi-files
	  (directory (cc-concat-paths
		     (directory-namestring midi-files)
		     "*.mid"))
	  analysis-file
	  :analyse-directory nil
	  :exclude-slots exclude-slots
	  :use-tempered-scale use-tempered-scale
	  :use-dynamic-symbols use-dynamic-symbols
	  :round-time-to-msecs round-time-to-msecs))
	(t
	 (format t "~&Analysing ~s..." midi-files)
	 (analyse (read-midi-file midi-files)
		  analysis-file
		  :exclude-slots exclude-slots
		  :use-tempered-scale use-tempered-scale
		  :use-dynamic-symbols use-dynamic-symbols
		  :round-time-to-msecs round-time-to-msecs)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SYNTHESIZE: USE ANALYSIS DATA TO AUTO-COMPLETE EVENTS        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Takes n values from an entry choosen by a given sort-predicate
;;; The result is a list like
;;; ((AMPLITUDE (AMP .5) .4) (PITCH (HZ 1) .1)...)
(defun cca-pick-values-from-entry-data (entry-data predicate amount)
  (let ((slot-name (car entry-data)))
    (loop for data in
	 (sort (copy-seq (cdr entry-data)) predicate :key #'cdr)
       repeat amount collect
	 (cons slot-name data))))

;;; returns values from analysis-data by a given entry-key
;;; (e.g.: (PITCH (HZ 10)) )
(defun cca-get-values-by-entry-key (analysis-data entry-key
				    &key
				      (amount 1)
				      (predicate #'>))
  (let ((values
	 (loop for entry in analysis-data
	    when (equal entry-key (car entry))
	    do (return (cdr entry)))))
    (loop for val in values append
	 (cca-pick-values-from-entry-data val predicate amount))))

;;; Auto-complete events with data found in an analysis file
;;; that match other data found in the events
;;; -> Meaning: This will only work, if any slot-values
;;;    are set, not for totally empty events.
;;; -> No subevents will be created! The structure of
;;;    the given events remains the same
(defun auto-complete-by-analysis-file (events analysis-file
				       &key
					 (determinacy 1)
					 (data-count-predicate #'>))
  (when (zerop determinacy)
    (setq determinacy .01))
  (let ((analysis-data
	 (with-open-file (file
			  analysis-file
			  :direction :input)
	   (read file)))
	(data-amount (/ 1 (* .01 (round (* 100 determinacy))))))
    (doevents (event events)
      (let ((fetched-data nil))
	;; fill fetched data with values from analysis file
	(loop for slot in (cc-get :event-slots) do
	     (let* ((slot-name (car slot))
		    (slot-val (funcall slot-name event)))
	       (when slot-val
		 (setq fetched-data
		       (append
			(cca-get-values-by-entry-key
			 analysis-data
			 (cca-make-key-obj slot-name slot-val)
			 :amount data-amount
			 :predicate data-count-predicate)
			fetched-data)))))
	;; fetched-data looks like this now:
	;; ((slot-name value-proc . amount) ...)
	(sort fetched-data data-count-predicate :key #'cddr)
	;; fill the event with data
	(loop for slot in (cc-get :event-slots) do
	     (let ((slot-name (car slot)))
	       (unless (funcall slot-name event)
		 (loop for fdat in fetched-data do
		      (when (and (equal (car fdat) slot-name)
				 ;; if det=1 this is always t
				 (< (random (float 1)) determinacy))
			(funcall slot-name event (eval (cadr fdat)))))))))))
  events)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ANALYSE VERSION 2: Analyse a fixed set of slots to determine ;;;
;;;                    one fixed slot, inspired by:              ;;;
;;; https://medium.com/technology-invention-and-more/            ;;;
;;; how-to-build-a-simple-neural-network-in-9-lines-of-python-   ;;;
;;; code-cc8f23647ca1                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cca-sigmoid (x)
  (/ 1 (+ 1 (exp (* -1 x)))))

(defun cca-sigmoid-derivative (x)
  (* x (- 1 x)))

(defun cca-think (input-values weights)
  (cca-sigmoid
   (sum 
    (mapcar #'* input-values weights))))

;; This is the actual training function!
(defun cca-train (input-values  ;; e.g. '((0 0 1) (1 0 1))
		  output-values ;; e.g. '(0 1)
		  &optional
		    (iterations 100)
		    weights)
  ;; We will always look at 4 sets of values at a time, meaning:
  ;; The previous/next events will influence the results.
  (unless weights
    (setq weights (loop for vals in output-values append '(1 1 1 1))))
  (when weights
    (setq weights (loop repeat 4 append weights)))
  (loop repeat iterations do
       (loop for in-vals1 in input-values
	  for in-vals2 in (cdr input-values)
	  for in-vals3 in (cddr input-values)
	  for in-vals4 in (cdddr input-values)
	  for correct-output in output-values
	  do
	    (let* ((in-vals (append in-vals1 in-vals2 in-vals3 in-vals4))
		   (computed-output (cca-think in-vals weights))
		   (error (- correct-output computed-output))
		   (adjustments
		    (loop for in-val in in-vals collect
			 (* in-val error
			    (cca-sigmoid-derivative computed-output)))))
	      (setq weights
		    (loop for w in weights
		       for a in adjustments collect
			 (+ w a))))))
  (loop for w in weights collect w))


(defun cca-get-min-max-slot-values (events
				    slot-name)
  "returns a list (min max unit) for values of slot-name"
  (let* ((min)
	 (max)
	 (unit
	  (doevents (e events)
	    (let ((val (funcall slot-name e)))
	      (when (unit-p val)
		(setq min (value val))
		(setq max (value val))
		(return (type-of val)))))))
    (doevents (e events)
      (let ((val (value e unit slot-name)))
	(when (< val min) (setq min val))
	(when (> val max) (setq max val))))
    (list min max unit)))

;; returns a list of lists per event, containing
;; one value per slot, always between 0 and 1,
;; relatively mapped according to the min and
;; max values in the events
(defun cca-get-relative-slot-values (events slots &optional min-max-ls)
  (let ((min-max-unit
	 (loop for s in slots collect
	      (cca-get-min-max-slot-values events s)))
	(result-ls))
    (doevents (e events)
      (push
       (loop for slot in slots
	  for (min max unit) in min-max-unit collect
	    (let ((result (value e unit slot)))
	      (if result
		  (shift-value result min max 0 1)
		  0)))
       result-ls))
    (reverse result-ls)))

(defun cca-train-simple-ai (training-events
			    slot-names
			    &key
			      (iterations 500)
			      start-weights)
  (format t "~&Training started...")
  (loop for target-slot in slot-names
     for nth-slot-name from 0 do
       (let* ((analysed-slots
	       (remove target-slot
		       (loop for slot in (cc-get :event-slots)
			  when (member 'numeric-unit slot)
			  collect (car slot))))
	      (weights (cca-train
			;; input-values
			(cca-get-relative-slot-values
			 training-events analysed-slots)
			;; output-values, must be flattened!
			(flat (cca-get-relative-slot-values
			       training-events (list target-slot)))
			iterations start-weights)))
	 (format t "~& Analysed weights for ~a: " target-slot)
	 (loop for w in weights
	    for slot in analysed-slots do
	      (format t " ~a ~a" slot w)))))

  
(defun auto-complete-events-by-simple-ai (events
					  slot-names
					  &key
					    training-events
					    (iterations 500)
					    overwrite-existing-values
					    weights
					    (remix-in-existing-values 0.0))
  ;; Cloned events will be used to access values.
  ;; This way, already modified values will not influence other slots.
  (let* ((cloned-events (clone events))
	 (all-events (if training-events
			 (list cloned-events training-events)
			 cloned-events)))
    (loop for target-slot in slot-names
       for nth-slot-name from 0 do
	 (let* ((analysed-slots
		 (remove target-slot
			 (loop for slot in (cc-get :event-slots)
			    when (member 'numeric-unit slot)
			    collect (car slot))))
		(weights (cca-train
			  ;; input-values
			  (cca-get-relative-slot-values
			   all-events analysed-slots)
			  ;; output-values, must be flattened!
			  (flat (cca-get-relative-slot-values
				 all-events (list target-slot)))
			  iterations weights))
		(min-max-unit
		 (cca-get-min-max-slot-values all-events target-slot))
		(min (first min-max-unit))
		(max (second min-max-unit))
		(unit (third min-max-unit))
		(tar-events-slot-vals
		 (cca-get-relative-slot-values
		  cloned-events analysed-slots)))
	   (doevents (e events)
	     (let ((existing-value (value e unit target-slot)))
	       (when (or (not existing-value)
			 overwrite-existing-values)
		 (let* ((input-values
			 (pop tar-events-slot-vals))
			(computed-factor
			 (cca-think input-values weights))
			(computed-value
			 (shift-value computed-factor 0 1 min max)))
		   (when existing-value
		     (setq computed-value
			   (+ 
			    (* computed-value
			       (abs (- 1 remix-in-existing-values)))
			    (* existing-value (clip remix-in-existing-values)))))
		   ;; (format t "~&Factor: ~a, Value: ~a~%" computed-factor computed-value)
		   (when unit (setq computed-value (funcall unit computed-value)))
		   (funcall target-slot e computed-value))))))))
  events)




;; (defun cca-get-weights-by-slot-names (events ;; The events to analyse
;; 				      target-slot ;; The slot to auto-complete
;; 				      analysed-slots ;; The slots to loo at
;; 				      &optional
;; 					(iterations 100)
;; 					weights)
;;   (let* ((slots (cons target-slot analysed-slots))
;; 	 (min-max-unit
;; 	  (loop for slot in slots collect
;; 	       (cca-get-min-max-slot-values events slot))))
;;     (unless weights
;;       (setq weights (loop for slot in analysed-slots collect 1)))
;;     (loop repeat iterations do
;; 	 (loop for event in events do
;; 	      (let* ((input-values
;; 		      (loop for slot-name in analysed-slots
;; 			 for (min max unit) in (cdr min-max-unit) collect
;; 			   (let ((result (value event unit slot-name)))
;; 			     (if result
;; 				 (shift-value result min max 0 1)
;; 				 0))))
;; 		     (target-value
;; 		      (value event (caddar min-max-unit) target-slot))
;; 		     (correct-output (shift-value target-value
;; 						  (caar min-max-unit)
;; 						  (cadar min-max-unit)
;; 						  0 1))
;; 		     (computed-output (cca-think input-values weights))
;; 		     (error (- correct-output computed-output))
;; 		     (adjustments
;; 		      (loop for in-val in input-values collect
;; 			   (* in-val error
;; 			      (cca-sigmoid-derivative computed-output)))))
;; 		(setq weights
;; 		      (loop for w in weights
;; 			 for a in adjustments collect
;; 			   (+ w a)))))))
;;   (loop
;;      for slot-name in analysed-slots
;;      for w in weights collect w))
 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
