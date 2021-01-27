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
;; render-modes/rm-isis.lsp                                                  2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/isis-mode
;;; Name
;;; isis-mode
;;;
;;; File
;;; rm-isis-mode.lsp
;;;
;;; Description
;;; A render-mode based on the IRCAMs ISiS (IRCAM Singing Synthesis).
;;; https://isis-documentation.readthedocs.io/en/latest/index.html
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
;;; Options
;;; voice: EL, MS or RT (in progress!)
;;;
;;; Dependencies
;;; :clm, isis
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
#+clm
(make-render-mode isis-mode :sound
		  :required-slots
		  (duration start-time text pitch)
		  :optional-slots
		  (amplitude location)
		  :options
		  (;; (voice "EL")
		   )
		  :required-packages (:clm)
		  :required-software (isis)
		  :group-events t
;;; ****
		  ;; because of the weired file structure of isis,
		  ;; everything must be done in :before-code
		  :before-code
		  (loop for e in (flat events) do
		       (let ((wav-file (make-tmp-file nil "wav"))
			     ;; WRITING FILE:
			     (isis-file
			      (make-tmp-file 
			       (with-output-to-string (file)
				 (let ((text-ls
					(loop for e in (flat event) collect
					     (text e)))
				       (midi-ls
					(loop for e in (flat event) collect
					     (%-> (pitch e) 0 127 'midinote)))
				       (duration-ls
					(loop for e in (flat event) collect
					     (%-> (duration e) .001 1000 'secs))))
				   (format file "[lyrics]~%xsampa: ")
				   (loop for elem in text-ls do
					(format file "~a " (value (xsampa elem))))
				   (format file "~%~%[score]~%midiNotes: ")
				   (format file "~{~a~^, ~}~%~%" midi-ls)
				   (format file "globalTransposition: 0~%~%")
				   (format file "rhythm: ") 
				   (format file "~{~a~^, ~}~%~%" duration-ls)
				   (format
				    file
				    "defaultSentenceLoudness: 0.2~%~%tempo: 60~%"))))))
			 ;; RUNNING ISIS
			 (run 'isis
			      "-m" isis-file
			      "-o" wav-file
			      ;; "-sv" voice
			      )
			 ;; Check if it worked
			 (if (probe-file wav-file)
			     (push (list (value e 'secs 'start-time)
					 (if (amplitude e)
					     (value e 'amp 'amplitude)
					     1)
					 (clm-get-amp-scalers
					  e protagonist)
					 wav-file)
				   tmp1)
			     (format
			      t
			      "~&ISIS-MODE: Rendering of event~
                                  with ID ~d failed."
			      (id e)))))
		  ;; MIXING FILES 
		  :footer-code
		  (let ((channels
			 (clm-get-channels ;;EDIT FUNCITION NAME
			  protagonist))
			(mix-file
			 (tmp-path
			  (format nil
				  "~a_isis.wav"
				  (name protagonist)))))
		    ;; mixing
		    (when tmp1
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
			  ,@(loop for (st amp scalers file) in tmp1 collect
				 `(clm::cc-samp5
				   ,file
				   ,(- st 1/10);rendered files will not start at 0
				   ,channels
				   :amp-scalers ,scalers
				   :amp ,amp))))
		      (setq return-file-path mix-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
