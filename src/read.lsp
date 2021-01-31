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
;; main/read.lsp                                                             2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-list-from-file (file)
  "Read first lisp-expression from a file, e.g. a list containing data."
  (with-open-file (file file)
    (read file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/load-and-return
;;; Name
;;; load-and-return
;;;
;;; File
;;; read.lsp
;;;
;;; Description
;;; Reads a lisp-expression from a file, evaluates it and returns the
;;; result. Used when loading a comic from a saved file, e.g. (save my-comic)
;;;
;;; Arguments
;;; file
;;;
;;; Return Value
;;; A saved object, e.g. a comic
;;;
;;; Example
#|
(save my-comic "/home/simon/my-comic.lsp") --> t
(load-and-return "/home/simon/my-comic.lsp") --> my-comic 
|#
;;;
;;; Last Modified
;;; 2021/01/22
;;;
;;; Synopsis
(defun load-and-return (file)
;;; ****
  "Reads a lisp-expression from a file, evaluates it and returns the
  result. Used when loading a comic from a saved file."
  (eval (read-list-from-file file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/read-midi-file
;;; Name
;;; read-midi-file
;;;
;;; File
;;; read.lsp
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
(defun read-midi-file (file &key tracks)
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
;;; OLD COMMON-MUSIC VERSION:
;; (defun read-midi-file (file &key
;; 			      (tracks t)
;; 			      tempo)
;;   "reads a midi file and returns a list of comic events per midi-note"
;;   (let* ((data
;; 	  (loop for track in (flat 
;; 			      (cm::import-events (namestring file)
;; 						 :tracks tracks
;; 						 :tempo tempo
;; 						 :time-format :beats
;; 						 ;;if any errors because of
;; 						 ;;meta-msgs happen,
;; 						 ;;add their names here...
;; 						 :meta-exclude t))
;; 	     append
;; 	       (slot-value track 'cm::subobjects)))
;; 	 (result))
;;     (loop for note in data do
;; 	 (when (typep note 'cm::midi)
;; 	   (push
;; 	    (make-event :amplitude
;; 			(midivelocity
;; 			 (round (* 127 (slot-value note 'cm::amplitude))))
;; 			:duration
;; 			(secs (slot-value note 'cm::duration))
;; 			:pitch
;; 			(midinote
;; 			 (round (slot-value note 'cm::keynum)))
;; 			:start-time
;; 			(secs (slot-value note 'cm::time)))
;; 	    result)))
;;     (reverse result)))

