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
;; render-modes/rm-music-xml-mode.lsp                                        2021 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/music-xml-mode
;;; Name
;;; music-xml-mode
;;;
;;; File
;;; rm-music-xml-mode.lsp
;;;
;;; Description
;;; 
;;;
;;; Output Format
;;; :notation
;;;
;;; Required Slots
;;; duration, start-time, pitch
;;;
;;; Optional Slots
;;; amplitude
;;;
;;; Last Modified
;;; 2021/04/05
;;;
;;; Synopsis
(make-render-mode music-xml-mode :notation
		  :required-slots
		  (start-time duration pitch)
		  :optional-slots
		  (amplitude text)
		  ;; :options
		  ;; (())
;;; ****
		  ;; all in header code, as we loop over bars rather
		  ;; than events here:
		  :header-code
		  (let ((file (make-tmp-file "" "musicxml")))
		    (with-open-file (file file
					  :direction :output
					  :if-exists :append)
		      (let ((current-tempo 0)
			    (current-ts 0))
			;;
			;; MUSIC XML HEADER
			;;
			(format file "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE score-partwise PUBLIC
    \"-//Recordare//DTD MusicXML 3.0 Partwise//EN\"
    \"http://www.musicxml.org/dtds/partwise.dtd\">
<score-partwise version=\"3.0\">
  <work>
    <work-title>~a</work-title>
  </work>
  <identification>
    <creator type=\"composer\">~a</creator>
    <encoding>
      <software>COMIC</software>
    </encoding>
  </identification>
  <part-list>
    <score-part id=\"P1\">
      <part-name>~a</part-name>
    </score-part>
  </part-list>
  <part id=\"P1\">"
				title
				(if author author "unknown")
				(name protagonist))
			;;
			;; BAR LOOP:
			;;
			(loop
			  for bar in bar-list
			  for bar-n from 1
			  do
			     ;; divisions 32: each quarter is
			     ;; divided into 32 steps. ->
			     ;; duration=32 is a quarter note
			     (format file "
    <measure number=\"~a\">
      <attributes>
        <divisions>32</divisions>
        <key>
          <fifths>0</fifths>
        </key>" bar-n)
			     ;;
			     ;; TIMESIG CHANGE?
			     ;;
			     (unless (= (slot-value bar 'ts)
					current-ts)
			       (setq current-ts (slot-value bar 'ts))
			       (format file "
        <time>
          <beats>~a</beats>
          <beat-type>~a</beat-type>
        </time>
      </attributes>"
				       (slot-value bar 'ts-nom)
				       (slot-value bar 'ts-denom)))
			     ;;
			     ;; TEMPO CHANGE?
			     ;;
			     (unless (= (slot-value bar 'tempo)
					current-tempo)
			       (setq current-tempo (slot-value bar 'tempo))
			       (format file "
       <direction placement=\"above\">
        <direction-type>
          <metronome>
            <beat-unit>quarter</beat-unit>
            <per-minute>~a</per-minute>
          </metronome>
        </direction-type>
        <sound tempo=\"~a\"/>
      </direction>" current-tempo current-tempo))
			     ;;
			     ;; NOTE LOOP
			     ;; IN PROGESS!!!
			     (let ((current-pos
				     (slot-value bar 'start-time))
				   (sum-divs
				     (* 32
					(slot-value bar 'beats))))
			       (loop
				 for e in
				       (get-events-in-time-range
					(slot-value bar 'start-time)
					(u+
					 (slot-value bar 'start-time)
					 (slot-value bar 'duration))
				    events)
				 do
				    ;;; SLICE EVENTS TO BAR:
				    (when (u> current-pos
					      (start-time e))
				      (let ((rest-divs
					      (round
					       (* 32 (value (- (value e 'secs 'start-time)
							current-pos)))))
					(format file "
      <note>
        <rest />
        <duration>~a</duration>
      </note>" rest-divs)

						
						(format file "
     </measure>"))
			(format file "
  </part>
</score-partwise>"))
		      (setq return-file-path file))))


		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF

     ;;    <pitch>
     ;;      <step>C</step>
     ;;      <octave>4</octave>
     ;;    </pitch>

     ;; <direction directive="yes" placement="above">
     ;;    <direction-type>
     ;;      <words default-y="15" font-weight="bold">Andantino</words>
     ;;    </direction-type>
     ;;    <sound tempo="60"/>
     ;;  </direction>
