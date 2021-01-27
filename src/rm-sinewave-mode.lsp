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
;; render-modes/rm-sinewave-mode.lsp                                         2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/sinewave-mode
;;; Name
;;; sinewave-mode
;;;
;;; File
;;; rm-sinewave-mode.lsp
;;;
;;; Description
;;; A basic render-mode for additive synthesis. Will render events as sinewaves
;;; to a soundfile, using the pitch, duration and start-time-slot. If amplitude
;;; is set, sinewaves will be scaled. Otherwise the amplitude defaults to 1.
;;; (The final result will be scaled anyway to prevent clipping.)
;;;
;;; Output Format
;;; :sound
;;;
;;; Required Slots
;;; pitch, duration, start-time
;;;
;;; Optional Slots
;;; amplitude
;;;
;;; Dependencies
;;; :clm
;;;
;;; Last Modified
;;; 2020/02/17
;;;
;;; Synopsis
#+clm
(make-render-mode sinewave-mode :sound
		  :required-slots
		  (duration start-time pitch)
		  :optional-slots
		  (amplitude)
		  :options
		  ((sample-rate 44100))
		  :required-packages (:clm)
;;; ****
		  :header-code
		  (let* ((out-file
			  (make-tmp-file
			   nil "wav"
			   (format nil "/sinewave-mode_~a" (name protagonist))))
			 (out-channels (clm-get-channels protagonist))
			 (inst-calls
			  (loop for e in (flat events) collect
			       (list
				'clm::additive-synthesis
				(%-> (pitch e) 0 22000 'hz)
				(if (amplitude e)
				    (%-> (amp (amplitude e)) 0 1 'amp)
				    1)
				(value (secs (duration e)))
				(value (secs (start-time e)))
				:amp-scalers (clm-get-amp-scalers
					       e protagonist)
				:out-channels out-channels)))
			 (ins-file (src-dir "clm-add-synth.lsp"))
			 (fasl-file (src-dir "clm-add-synth.fasl")))
		    ;; ;; make sure the output-dir exists
		    ;; (ensure-directories-exist out-file)
		    ;; only compile file if not already compiled
		    (if (probe-file fasl-file)
			(load (src-dir "add-synth"))
			(load (compile-file ins-file)))
		    (eval `(clm::with-sound
			       (:output ,out-file
					:srate ,sample-rate
					:clipped nil
					:play nil
					:channels ,out-channels
					:header-type clm::mus-riff
					:data-format clm::mus-lfloat)
			     ,@inst-calls))
		    (setq return-file-path out-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
