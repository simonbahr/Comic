;c;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; render/make-notation-data.lsp                                             2021 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

(defclass cc-bar ()
  ((ts :initarg :ts :initform nil)
   (ts-nom :initarg :ts-nom :initform nil)
   (ts-denom :initarg :ts-denom :initform nil)
   (beats :initarg :beats :initform nil)
   (start-time :initarg :start-time :initform nil)
   (duration :initarg :duration :initform nil)
   (tempo :initarg :tempo :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-tempo (vals)
  "Takes a collection of start-times and tries to find a good common tempo."
  (labels ((get-dev (val tempo c)
	     (multiple-value-bind (beat dev)
		 (round (/ val (/ tempo 60)))
	       (declare (ignore beat))
	       (let ((the-dev (abs dev)))
		 (if (< 0 c )
		     (+ (* 2 the-dev)
			(get-dev the-dev (* 2 tempo) (1- c)))
		     the-dev))))
	   (get-beat-deviation-sum (tempo)
	     (loop for val in vals
		summing
		  (get-dev val tempo 3)
		into result
		finally (return result))))
    (cadar
     (sort 
      (loop for tempo from 60 below 120 collect
	   (let ((dev (get-beat-deviation-sum tempo)))
	     (list dev tempo)))
      #'< :key #'car))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-get-beats-amount (tempo min-duration)
  "how many beats at given tempo last at least min-duration?"
  (let ((beat-duration (/ 60 tempo)))
    (ceiling (/ min-duration beat-duration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-tempo-beat-map (times)
  "returns a list of 2-element-lists containing 1. a tempo
   and 2. a number of beats (quarter-notes)"
  (let ((current-pos 0)
	(tempo-beat-map))
    ;; fill the tempo-beat-map
    (loop while (sort times #'<)
	  do
	     (let* ((len (length times)))
	       (let* ((i-init (min 3 len))
		      ;; get init tempo from next 3 or less events:
		      (tempo (find-tempo (subseq times 0 i-init)))
		      ;; how many events with same tempo?
		      (i (loop for i from i-init
			       repeat (length times)
			       unless (= tempo
					 (find-tempo (save-subseq times 0 i)))
				 do
				    (return i)
			       finally (return i-init)))
		      ;; calc number of beats that the tempo stays the same:
		      (beats (cc-get-beats-amount
			      tempo
			      (- (nth (1- i) times) current-pos))))
		 (push (list tempo beats) tempo-beat-map)
		 (setq times (subseq times i))
		 (setq current-pos (* (/ 60 tempo) beats)))))
    ;; reverse
    (setq tempo-beat-map (reverse tempo-beat-map))
    ;; remove double entries and return:
    (loop for (tempo beats) in tempo-beat-map
	  with prev-tempo = (caar tempo-beat-map)
	  with prev-beats = 0
	  when (= tempo prev-tempo)
	    do
	       (incf prev-beats beats)
	  when (not (= tempo prev-tempo))
	    collect
	    (prog1
		(list prev-tempo prev-beats)
	      (setq prev-tempo tempo)
	      (setq prev-beats beats))
	    into result
	  finally
	     (return (append
		      result
		      `((,prev-tempo ,prev-beats)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-timesig (vals tempo &optional (offset 0) max-num-beats)
  "Takes a collection of start-times (in beats!) and tries to find a 
   good common timesignature."
  ;; The best timesig is choosen so that the next bar will ideally
  ;; start with an event, not with a rest!
  ;;
  ;; first, pre-select the possible timesigs:
  (let ((timesigs '(1 1/2 3/4 3/2 5/4 3/8 5/8 7/8 9/8)))
    (when max-num-beats
      (setq timesigs
	    (loop for ts in timesigs
		  when (<= (* 4 ts) max-num-beats)
		    collect ts)))
    ;; helper function that gets the devation for a given timesig:
    (labels ((get-dev (ts)
	       (let* ((bar-end (+ offset (* (/ 60 tempo) ts 4))))
		 (loop for val in vals
		       minimize (abs (- bar-end val))
			 into dev
		       finally (return dev)))))
      (cadar
       (sort 
	(loop for ts in timesigs
	      collect
	      (let ((dev (get-dev ts)))
		(list dev ts)))
	#'< :key #'car)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-decode-timesig (val)
  "a timesig can be saved as a single value (num hole notes) and 
   will be decoded to its simplest representation, returning the
   nominator and denominator as two values"
  (let ((denom 1))
    ;; simplest special case: 4/4
    (if (= val 1)
	(values 4 4)
	;; otherwise double nom and denom until nom is a whole number:
	(loop repeat 64
	      do
		 (setq val (* 2 val)
		       denom (* 2 denom))
		 (when (and (= (round val) val) (> val 1))
		   (return (values (round val) denom)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-cc-bar-list (comic)
  "returns a list of 2-element-lists representing a bar each,
       containing 1. a tempo in bpm and 2. a timesig, encoded as
       number of whole-notes"
  ;; collect all start and end times of events in a list:
  (let ((times)
	(current-pos 0)
	(current-ts 1))
    (doevents (e comic)
      (let ((st (value e 'secs 'start-time)))
	(when st (push st times))))
    (setq times (sort times #'<))
    ;; make the tempo-beat-map
    (let* ((tempo-beat-map (make-tempo-beat-map times))
	   (bar-map
	     ;; make the bar map:
	     (loop for (tempo beats) in tempo-beat-map
		   append
		   (loop while (> beats 0)
			 with ts = current-ts
			 repeat 100
			 unless ;;empty bar? do not change ts!
			 (> (car times)
			    (+ current-pos (* (/ 60 tempo) ts 4)))
			 do
			    (if (< beats 4)
				(setq ts (/ beats 4))
				(setq ts (find-timesig (save-subseq times 0 16)
						       tempo current-pos
						       beats)))
			 collect
			 (let* ((beats (* ts 4))
				(bar-dur (* (/ 60 tempo) beats)))
			   (prog1 
			       (multiple-value-bind (nom denom)
				   (cc-decode-timesig ts)
				 (make-instance 'cc-bar
						:ts ts
						:ts-nom nom
						:ts-denom denom
						:beats beats
						:tempo tempo
						:start-time
						(secs current-pos)
						:duration
						(secs (+ current-pos
							  bar-dur))))
			     (decf beats (* current-ts 4))
			     (incf current-pos bar-dur)))))))
      ;; return values:
      (values
       ;; 1. the cc-bar-list, holding
       ;; tempo-and-timesig lists per par
       bar-map
       ;; 2. tempo map: tempo changes and times in secs
       (let ((pos 0))
	 (loop for (tempo beats) in tempo-beat-map
	       collect
	       (prog1
		   (list tempo pos)
		 (incf pos (* (/ 60 tempo) beats)))))
       ;; 3. a timesig-map: timesig-changes and times
       ;; in secs
       (let ((pos 0)
	     (ts 0))
	 (loop for bar in bar-map
	       for the-ts = (slot-value bar 'ts)
	       unless (= ts the-ts)
		 collect
		 (progn (setq ts the-ts) (list ts pos))
		 into result
	       do (incf pos (value (slot-value bar 'duration) 'secs))
	       finally (return result)))))))
		   
;; (defmethod get-cc-bar ((event event) cc-bar-list)
;;   (let ((st (start-time event))
;; 	(dur (duration event)))
;;     (when (and st dur)
;;       (loop for bar in cc-bar-list
;; 	    when (and (u> st (slot-value bar 'start-time))
;; 		      (u< (u+ st dur) (
