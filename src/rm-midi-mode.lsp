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
;; render-modes/rm-midi-mode.lsp                                             2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/midi-mode
;;; Name
;;; midi-mode
;;;
;;; File
;;; rm-midi-mode.lsp
;;;
;;; Description
;;; A render-mode for simple midi output.
;;; Pitch, start-time and duration will be used to create midinotes on a single
;;; channel (Cannel 1) of the output-file. Amplitude values will be used when
;;; given, else midivelocity will default to 80.
;;;
;;; Output Format
;;; :midi
;;;
;;; Required Slots
;;; duration, start-time, pitch
;;;
;;; Optional Slots
;;; amplitude
;;;
;;; Last Modified
;;; 2021/01/31
;;;
;;; Synopsis
(make-render-mode midi-mode :midi
		  :required-slots
		  (start-time duration pitch)
		  :options
		  ((division 480))
		  :optional-slots
		  (amplitude)
;;; ****
		  :event-code
		  (progn
		    (push
		     (make-instance 'midi:note-off-message
		    		    :status 128
		    		    :velocity 0
		    		    :key (round (value pitch 'midinote))
		    		    :time (1- (round
					       (midi::get-midi-time
						(+
						 (value duration 'secs)
						 (value start-time 'secs))
						division))))
		     tmp1)
		    (push
		     (make-instance 'midi:note-on-message
				    :status 144
				    :velocity (round
					       (if amplitude
						   (value amplitude
							  'midivelocity)
						   80))
				    :key (round (value pitch 'midinote))
				    :time (round
					   (midi::get-midi-time
					    (value start-time 'secs)
					    division)))
		     tmp1))
		  :footer-code
		  (let ((outfile
			  (make-tmp-file nil "mid" "midi-mode")))
		    ;;write the midi file:
		    (midi:write-midi-file
		     (make-instance 'midi:midifile
				    :division 480
				    :format 0
				    :tracks
				    (list
				     (append
				      (list
				       (make-instance
					'midi::tempo-message
					:status 255
					:time 0
					:tempo 500000))
				      (sort tmp1
					    #'< :key
					    #'midi:message-time))))
		     outfile)
		    ;; success?
		    (when (probe-file outfile)
		      (setq return-file-path outfile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/midi-notation-mode
;;; Name
;;; midi-notation-mode
;;;
;;; File
;;; rm-midi-mode.lsp
;;;
;;; Description
;;; A render-mode for notateable midi output.
;;; Pitch, start-time and duration will be used to create midinotes on a single
;;; channel (Cannel 1) of the output-file. Amplitude values will be used when
;;; given, else midivelocity will default to 80.
;;;
;;; The midi timing is not optimal yet! There are deviations in
;;; output-times, especially when writing long files.
;;; When Syncronisation with other modes is important, it is
;;; recommendable to e.g. generate midi snippets for single sections
;;; for now, instead of one long file.
;;;
;;; Output Format
;;; :midi
;;;
;;; Required Slots
;;; duration, start-time, pitch
;;;
;;; Optional Slots
;;; amplitude
;;;
;;; Last Modified
;;; 2021/01/31
;;;
;;; Synopsis
(make-render-mode midi-notation-mode :midi
		  :required-slots
		  (start-time duration pitch)
		  :options
		  ((division 192))
		  :optional-slots
		  (amplitude)
		  :make-notation-data t
;;; ****
		  :header-code
		  ;; (setq tmp1 (make-midi-tempo-timesig-changes bar-list
		  ;; 					      events
		  ;; division))
		  (let ((protagonist-end
			  (loop for e in events
				maximize
				(value (u+ (start-time e)
					   (duration e))
				       'secs))))
			;;
			;; TEMPO CHANGES
			;;
			(loop for (tempo time) in tempo-map
			      when
				(<= time protagonist-end)
			      do
				 (push
				  (make-instance
				   'midi:tempo-message
				   :tempo
				   (midi::bpm->microtempo tempo)
				   :status 255
				   :time
				   (floor
				    (midi::get-midi-time-by-tempo-map
				     time division tempo-map)))
				  tmp1))
			;;
			;; TIMESIG CHANGES
			;;
			(loop for (ts time) in timesig-map
			      when
			      (<= time protagonist-end)
			      do
				 (multiple-value-bind (nn dd)
				     (cc-decode-timesig ts)
				   (push
				    (make-instance
				     'midi:time-signature-message
				     :nn nn
				     :dd (case dd (2 1) (4 2) (8 3)
					       (16 4) (32 5) (64 6)
					       (otherwise 2))
				     :cc division :bb 8
				     :status 255
				     :time
				     (floor
				      (midi::get-midi-time-by-tempo-map
				       time division tempo-map)))
				    tmp1))))
		  :event-code
		  (progn
		    (push
		     (make-instance
		      'midi:note-off-message
		      :status 128
		      :velocity 0
		      :key (round (value pitch 'midinote))
		      :time ;; (1-
			     (floor
			      (midi::get-midi-time-by-tempo-map
			       (+
				(value duration 'secs)
				(value start-time 'secs))
			       division tempo-map)));)
		     tmp1)
		    (push
		     (make-instance
		      'midi:note-on-message
		      :status 144
		      :velocity (round
				 (if amplitude
				     (value amplitude
					    'midivelocity)
				     80))
		      :key (round (value pitch 'midinote))
		      :time 
		      (floor (midi::get-midi-time-by-tempo-map
		       (value start-time 'secs)
		       division tempo-map)))
		     tmp1))
		  :footer-code
		  (let ((outfile
			  (make-tmp-file nil "mid" "midi-mode")))
		    ;;write the midi file:
		    (midi:write-midi-file
		     (make-instance 'midi:midifile
				    :division division
				    :format 0
				    :tracks
				    (list
				     (sort tmp1
					   #'< :key
					   #'midi:message-time)))
		     outfile)
		    ;; success?
		    (when (probe-file outfile)
		      (setq return-file-path outfile))))

;; (defun get-midi-time-by-cc-bars (secs division cc-bars)
;;   (let ((time 0)
;; 	(midi-time 0))
;;     (loop for bar in cc-bars
;; 	  do
;; 	     (let* ((tempo (slot-value bar 'tempo))
;; 		    ;; (beats (slot-value bar 'beats))
;; 		    (bar-dur
;; 		      (value (slot-value bar 'duration)
;; 			     'secs)))
;; 	       (if (< time secs (+ bar-dur time))
;; 		   (let ((remaining (- secs time)))
;; 		     (incf time remaining)
;; 		     (incf midi-time
;; 			   (midi::get-midi-time
;; 			    remaining
;; 			    division tempo)))
;; 		   (when (< time secs)
;; 		     (incf time bar-dur)
;; 		     (incf midi-time
;; 			   (midi::get-midi-time
;; 			    bar-dur division tempo))))))
;;     midi-time))

;; (defun make-midi-tempo-timesig-changes (cc-bars events division)
;;   (let ((protagonist-end
;; 	  (loop for e in events
;; 		maximize
;; 		(value (u+ (start-time e)
;; 			   (duration e))
;; 			   'secs)))
;; 	(the-tempo 0)
;; 	(the-timesig 1)
;; 	(time 0)
;; 	(result))
;;     (loop for bar in cc-bars
;; 	  do
;; 	     (let* ((tempo (slot-value bar 'tempo))
;; 		    (timesig (slot-value bar 'ts))
;; 		    (beats (slot-value bar 'beats))
;; 		    (bar-dur (* beats (/ 60 tempo))))
;; 	       (unless (or (= tempo the-tempo)
;; 			   (>= time protagonist-end))
;; 		 (push
;; 		  (make-instance
;; 		   'midi:tempo-message
;; 		   :tempo
;; 		   (midi::bpm->microtempo tempo)
;; 		   :status 255
;; 		   :time
;; 		   (floor
;; 		    (midi::get-midi-time
;; 		     time division tempo)))
;; 		  result)
;; 		 (setq the-tempo tempo))
;; 	       (unless (or (= timesig the-timesig)
;; 			   (>= time protagonist-end))
;; 		 (multiple-value-bind (nn dd)
;; 		     (cc-decode-timesig the-timesig)
;; 		   (push
;; 		    (make-instance
;; 		     'midi:time-signature-message
;; 		     :nn nn
;; 		     :dd (case dd (2 1) (4 2) (8 3)
;; 			       (16 4) (32 5) (64 6)
;; 			       (otherwise 2))
;; 		     :cc division :bb 8
;; 		     :status 255
;; 		     :time
;; 		     (floor
;; 		      (midi::get-midi-time
;; 		       time division tempo)))
;; 		    result))
;; 		 (setq the-timesig timesig))
;; 	       (incf time bar-dur)))
;;     result))

;;; OLD COMMON MUSIC VERSION:
;; #+cm
;; (make-render-mode midi-mode :midi
;; 		  :required-slots
;; 		  (start-time duration pitch)
;; 		  :optional-slots
;; 		  (amplitude)
;; 		  :required-packages (:cm)
;; 		  :header-code
;; 		  (let ((outfile
;; 			  (make-tmp-file nil "mid" "midi-mode")))
;; 		    (cm::events
;; 		     (loop for e in events
;; 			   collect
;; 			   (cm::new cm::midi
;; 			     :time (float (value e 'secs 'start-time))
;; 			     :duration (float (value e 'secs 'duration))
;; 			     :keynum (round (value e 'midinote 'pitch))
;; 			     :amplitude (float
;; 					 (let ((val (value e 'amp 'amplitude)))
;; 					   (if val
;; 					       val
;; 					       (/ 80.0 127))))))
;; 		     outfile)
;; 		    (when (probe-file outfile)
;; 		      (setq return-file-path outfile))))

;; (defun get-midi-time-by-tempo-map
;;     (secs divisions &optional (tempo-map '((60 0))))
;;   (let ((current-secs 0)
;; 	(current-time 0))
;;     (loop for (tempo temposecs) in tempo-map
;; 	  unless (> secs temposecs)
;; 	    do
;; 	       (incf current-secs temposecs)
;; 	       (incf current-time
;; 		     (* (/ (- temposecs current-secs)
;; 			   (/ 60 tempo))
;; 			24))
;; 	  finally
;; 	     (incf current-time
;; 		   (* (/ (- secs current-secs)
;; 			 (/ 60 tempo))
;; 		      24)))
;;     (floor current-time)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF

