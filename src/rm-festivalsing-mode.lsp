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
;; render-modes/rm-festivalsing-mode.lsp                                     2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/festivalsing-mode
;;; Name
;;; festivalsing-mode
;;;
;;; File
;;; rm-festivalsing-mode.lsp
;;;
;;; Description
;;; A text-to-voice render-mode, using festivals singing-mode,
;;; executed by the subprogram text2wave. For more info on festival
;;; visit http://festvox.org/
;;;
;;; Output Format
;;; :sound
;;;
;;; Required Slots
;;; text, pitch, duration, start-time
;;;
;;; Optional Slots
;;; amplitude, location
;;;
;;; Dependencies
;;; :clm, festival/text2wave
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
#+clm
(make-render-mode festivalsing-mode :sound
		  :required-slots
		  (duration start-time text pitch)
		  :optional-slots
		  (location amplitude)
		  ;; :options
		  ;; ((voice "en1_mbrola"))
		  :required-packages (:clm)
		  :required-software (festival/text2wave)
;;; ****
		  :event-code
		  (let ((xml-file-name (make-tmp-file nil "xml"))
			(wav-file (make-tmp-file nil "wav")))
		    (with-open-file (xml-file xml-file-name
					      :direction :output)
		      ;; Write xml-data:
		      (format xml-file "<?xml version=\"1.0\"?> ~
<!DOCTYPE SINGING PUBLIC \"-//SINGING//DTD SINGING mark up//EN\" ~
\"Singing.v0_1.dtd\" []> 
<SINGING BPM='60'>~%
<PITCH FREQ='~a'><DURATION ~
SECONDS='~a'>~a</DURATION></PITCH>
</SINGING>"
			      (round (value pitch 'hz))
			      (value duration 'secs)
			      (value text)))
		    ;; RUNNING FESTIVAL
		    (run 'festival/text2wave
			 "-mode" "singing" ;; "-scale" "1"
			 xml-file-name
			 "-o" wav-file)
		    ;; Check if it worked
		    (if (probe-file wav-file)
			(push (list (value start-time 'secs)
				    (if amplitude
					(value amplitude 'amp)
					1)
				    (clm-get-amp-scalers
				     event protagonist)
				    wav-file)
			      tmp2)
			(format
			 t
			 "~&FESTIVALSING-MODE: Rendering of event~
                                  with ID ~a failed." id)))
		  ;; MIXING FILES 
		  :footer-code
		  (let ((channels
			  (clm-get-channels ;;EDIT FUNCITION NAME
			   protagonist))
			(mix-file
			  (tmp-path
			   (format nil
				   "~a_festivalsing.wav"
				   (name protagonist)))))
		    ;; mixing
		    (when tmp2
		      (eval
		       `(clm::with-sound
			    (:output
			     ,mix-file
			     :clipped nil
			     :play nil
			     :channels ,channels
			     :header-type clm::mus-riff
			     :data-format clm::mus-lfloat)
			  ;; add channels/spacial positioning
			  ,@(loop for (st amp scalers file) in tmp2 collect
				  `(clm::cc-samp5
				    ,file
				    ,st
				    ,channels
				    :amp-scalers ,scalers
				    :amp ,amp))))
		      (setq return-file-path mix-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
