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
;; main/midi.lsp                                                             2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/read-midi-file
;;; Name
;;; read-midi-file
;;;
;;; File
;;; midi.lsp
;;;
;;; Description
;;; Read notes from a midi-file as comic-events, meaning:
;;; - midinotes -> pitch-slot,
;;; - midivelocity -> amplitude-slot,
;;; - ...as well as start-time and duration.
;;;
;;; N.B.: Requires common-music, as e.g. included in
;;; slippery-chicken.
;;;
;;; Arguments
;;; file
;;;
;;; Return Value
;;; A list of events
;;;
;;; Last Modified
;;; 2020/04/20
;;;
;;; Synopsis
#+cm
(defun read-midi-file (file &key
			      (tracks t)
			      tempo)
  "reads a midi file and returns a list of comic events per midi-note"
;;; ****
  (let* ((data
	  (loop for track in (flat 
			      (cm::import-events (namestring file)
						 :tracks tracks
						 :tempo tempo
						 :time-format :beats
						 ;;if any errors because of
						 ;;meta-msgs happen,
						 ;;add their names here...
						 :meta-exclude t))
	     append
	       (slot-value track 'cm::subobjects)))
	 (result))
    (loop for note in data do
	 (when (typep note 'cm::midi)
	   (push
	    (make-event :amplitude
			(midivelocity
			 (round (* 127 (slot-value note 'cm::amplitude))))
			:duration
			(secs (slot-value note 'cm::duration))
			:pitch
			(midinote
			 (round (slot-value note 'cm::keynum)))
			:start-time
			(secs (slot-value note 'cm::time)))
	    result)))
    (reverse result)))
