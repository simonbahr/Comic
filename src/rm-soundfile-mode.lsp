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
;; render-modes/rm-soundfile-mode.lsp                                        2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/soundfile-mode
;;; Name
;;; soundfile-mode
;;;
;;; File
;;; rm-soundfile-mode.lsp
;;;
;;; Description
;;; A basic render-mode for sampled sounds. The soundfiles linked to in the
;;; soundfile-slot of events will be placed in time. If soundfile is a
;;; soundfile-object an has a duration and/or start-time, it will be cut
;;; by these values first.
;;;
;;; If an amplitude is given, the sound will be scaled by that amplitude.
;;;
;;; When src = t (the default) and the soundfile is a soundfile-object 
;;; with a given pitch and the event has a pitch, the resulting sound will
;;; be sped up or slowed down using sample-rate-conversion, with the 
;;; soundfiles pitch as reference and the events pitch as target.
;;;
;;; When the protagonist has a non-zero expansion, it is considered to
;;; represent an array of multiple loudspeakers. Events are placed and scaled
;;; on each channel depending on their spatial position. An expansion of nil 
;;; or 0 will result in a mono-file, a 1-dimensional protagonist will
;;; result in a stereo-file, a 2-dimensional protagonist will result in a
;;; 4-channel file, and so forth (2^dims-protagonist = channels).
;;; 
;;; Output Format
;;; :sound
;;;
;;; Required Slots
;;; soundfile, duration, start-time
;;;
;;; Optional Slots
;;; pitch, amplitude, location, expansion
;;;
;;; Options
;;; src (default: t)
;;; srt-width (default: 5, depth of src)
;;; srate (default: 44100)
;;;
;;; Dependencies
;;; :clm
;;;
;;; Last Modified
;;; 2020/02/17
;;;
;;; Synopsis
#+clm
(make-render-mode soundfile-mode :sound
		  :required-slots
		  (soundfile duration start-time)
		  :optional-slots
		  (pitch amplitude)
		  :options
		  ((src t) (srt-width 5) (srate 44100))
		  :required-packages (:clm)
;;; ****
		  :event-code
		  (let* ((file (value soundfile))
			 (beg (value start-time 'secs))
			 (dur
			  (value duration 'secs))
			 (start 
			  (value (start-time soundfile) 'secs))
			 (sf-dur (value (duration soundfile) 'secs))
			 (end
			  (if sf-dur (+ sf-dur start) 0))
			 (amp (if amplitude (value amplitude 'amp) 1.0))
			 (frq (value pitch 'hz))
			 (sf-frq (value (pitch soundfile) 'hz))
			 (srt (if (and src frq sf-frq)
				  (if (zerop sf-frq)
				      0
				      (/ frq sf-frq))
				  1))
			 ;; make the instrument-call
			 (call
			  `(clm::cc-samp5
			    ,file ,beg
			    ,(clm-get-channels protagonist)
			    :duration ,dur
			    :snd-dur ,sf-dur
			    :start ,start
			    :end ,end
			    :srt ,(if srt srt 1)
			    :amp ,amp
			    :width ,srt-width
			    :amp-scalers
			    ,(clm-get-amp-scalers
			      event protagonist))))
		    ;; (format t "~&beg: ~a, dur: ~a, st: ~a, sf-dur: ~a, end: ~a"
		    ;; 	    beg dur start sf-dur end)
		    ;; (print (soundfile-mode-get-amp-scalers
		    ;; 	      event protagonist))
		    (unless (or
			     (zerop amp)
			     (zerop srt))
		      (push call tmp1)))
		  :footer-code
		  (let ((out-file
			 (make-tmp-file
			  nil "wav"
			  (format nil "soundfile-mode-~a"
				  (name protagonist)))))
		    (eval `(clm::with-sound
			       (:output ,out-file
					:clipped nil
					:scaled-to 1
					:play nil
					:srate ,srate
					:continue-old-file nil
					:channels ,(clm-get-channels protagonist))
			     ,@tmp1))
			  (setq return-file-path out-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
