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
		    		    :time (1-
					   (midi::get-midi-time
					    (+
					     (value duration 'secs)
					     (value start-time 'secs))
					    division)))
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
				    :time (midi::get-midi-time
					   (value start-time 'secs)
					   division
					   ;; add tempo here?
					   ))
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
				       ;; (make-instance
				       ;; 	'midi::sequence/track-name-message
				       ;; 	:time 0
				       ;; 	:status 255
				       ;; 	)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
