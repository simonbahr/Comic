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
;; main/event-read.lsp                                                       2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/midifile-to-events
;;; Name
;;; midifile-to-events
;;;
;;; File
;;; event-read.lsp
;;;
;;; Description
;;; Read notes from a midi-file as comic-events, meaning:
;;; - midinotes -> pitch-slot,
;;; - midivelocity -> amplitude-slot,
;;; - ...as well as start-time and duration.
;;;
;;; Arguments
;;; file
;;;
;;; Return Value
;;; A list of events
;;;
;;; Last Modified
;;; 2021/01/31
;;;
;;; Synopsis
(defun midifile-to-events (file &key tracks)
  "reads a midi file and returns a list of comic events per midi-note"
;;; ****
  (let* ((midifile
	   (midi:read-midi-file file))
	 (midifile-contents
	   (slot-value midifile 'midi::tracks))
	 (midi-events (if tracks
			  (loop for track in (flat tracks)
				append
				(nth (1- track) midifile-contents))
			  (flat midifile-contents)))
	 (division (slot-value midifile 'midi::division))
	 (microseconds-per-beat 500000) ; default, =120bpm
	 (events))
    ;; loop through all midi events and find notes:
    (loop for m in midi-events
	  for rem on (cdr midi-events)
	  do
	     (cond (;; when tempo-message, set tempo
		    (typep m 'midi:tempo-message)
		    (midi::get-bpm m))
		   (;; make a new event for each note-on-message
		    (and (typep m 'midi:note-on-message)
			 (not (zerop (midi:message-velocity m))))
		    (let* (;; simple values are:
			   (st (midi::get-time-in-secs
				(midi:message-time m)
				microseconds-per-beat
				division))
			   (ptch (midi:message-key m))
			   (amp (midi:message-velocity m))
			   ;; now loop in remaining messages to find
			   ;; next note-off on same key:
			   (end (loop for rm in rem
				      when
				      (and
				       (midi::is-note-off rm)
				       (= ptch
					  (midi:message-key rm)))
				      return
				      (midi::get-time-in-secs
				       (midi:message-time rm)
				       microseconds-per-beat
				       division))))
		      (push
		       (make-event :start-time (when st (secs st))
				   :duration (when (and st end)
					       (secs (- end st)))
				   :pitch (when ptch (midinote ptch))
				   :amplitude (when amp (midivelocity amp)))
		       events)))))
    (reverse events)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/soundfile-to-events
;;; Name
;;; soundfile-to-events
;;;
;;; File
;;; event-read.lsp
;;;
;;; Description
;;; Analyses melodic, harmonic and rhythmic content of a soundfile and
;;; tries to create nice chords on the basis of this analysis,
;;; returned as comic-events. Totally glitchy, but lots of fun!
;;;
;;; Arguments
;;; file
;;;
;;; Return Value
;;; A list of events
;;;
;;; Last Modified
;;; 2021/02/16
;;;
;;; Synopsis
#+aubio
(defun soundfile-to-events (file &key (max-notes-per-chord 1))
  "Analysis a soundfile and creates some funky events!" 
;;; ****
  (labels ((run-aubio-analysis (&rest args)
	     (with-input-from-string
		 ;; read aubio output with given args:
		 (str (apply #'run-and-return-output 'aubio args))
	       ;; collect lines into lists, holding...
	       (loop
		 for ln = (read-line str nil nil)
		 while ln
		 collect
		 ;; (float) numbers:
		 (with-input-from-string (s ln)
		   (loop
		     for num = (read s nil nil)
		     while num
		     collect num)))))
	   ;; util for better voicings:
	   (midinote-in-range (mnote reference-mnote)
	     (loop until (< (abs (- mnote reference-mnote)) 12)
		   do (setq mnote (if (> mnote reference-mnote)
				      (- mnote 12)
				      (+ mnote 12)))
		   finally (return mnote))))
    (let ((notes
	    (loop for n in (run-aubio-analysis "notes" file)
		  ;; must hold pitch, st, end:
		  when (and (= 3 (length n))
			    (all? 'numberp n)
			    ;;... and not be too short
			    (> (- (third n) (second n)) 0.01))
		    collect n))
	  (pitches
	    (loop for n in (run-aubio-analysis
			    "pitch"
			    "-t" "0.9"
			    "-H" "1024" file)
		  when (all? 'numberp n)
		    collect n))
	  (prev-mnote))
      (loop for (mnote st end) in notes
	    collect
	    (let ((dur (- end st))
	    	  ;; find some more pitches in time-frame of note:
	    	  (mnotes
	    	    (loop for i from 0
	    		  for (time freq) in pitches
	    		  until (> time end)
	    		  when (> time st)
	    		    collect
	    		    (value (midinote (hz freq)))
	    		    into result
	    		  finally (progn (setq pitches
	    				       (nthcdr i pitches))
	    				 (return result)))))
	      ;; make sure the melody does not jump too much
	      (when prev-mnote
	      	(setq mnote (midinote-in-range mnote prev-mnote)))
	      (setq prev-mnote mnote)
	      (if mnotes
		  (let ((selected-notes
			  ;; (in case bass-note is member of pick-n:)
			  (remove-duplicates
			   (cons (apply #'min mnotes) ; bass-note
				 (cons mnote ; melody-note
				       (pick-n (1- max-notes-per-chord)
					       mnotes))))))
		    ;; make sure no note is way above melody:
		    (setq selected-notes
			  (loop for n in selected-notes
				collect
				(if (> n mnote)
				    (midinote-in-range n mnote)
				    n)))
		    (make-event :start-time (secs st)
				:duration (secs dur)
				:events
				(loop
				  for note in (sort selected-notes #'>)
				  repeat max-notes-per-chord
				  collect
				  (make-event :pitch (midinote note)
					      :start-time 0))))
		  (make-event :start-time (secs st)
			      :duration (secs dur)
			      :pitch mnote)))))))
