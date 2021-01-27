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
;;; Dependencies
;;; :cm
;;;
;;; Last Modified
;;; 2020/04/20
;;;
;;; Synopsis
#+cm
(make-render-mode midi-mode :midi
		  :required-slots
		  (start-time duration pitch)
		  :optional-slots
		  (amplitude)
		  :required-packages (:cm)
;;; ****
		  :header-code
		  (cm::events
		   (loop for e in events collect
			(cm::new cm::midi
			  :time (float (value e 'secs 'start-time))
			  :duration (float (value e 'secs 'duration))
			  :keynum (round (value e 'midinote 'pitch))
			  :amplitude (float
				      (let ((val (value e 'amp 'amplitude)))
					(if val val (/ 80.0 127))))))
		   (ensure-directories-exist
		    (absolute-path
		     (format nil 
			     "~a-midi-mode.mid"
			     (name protagonist)))))
		  :footer-code
		  (let ((outfile (absolute-path
				  (format nil 
					  "~a-midi-mode.mid"
					  (name protagonist)))))
		    ;; when midi-mixing is implemented,
		    ;; return path, but write the file to tmp-dir
		    (when (probe-file outfile)
		      (setq return-file-path t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
