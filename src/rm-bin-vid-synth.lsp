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
;; render-modes/rm-bin-vid-synth.lsp                                         2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * * * * R * render-modes/bin-vid-synth
;;; Name
;;; bin-vid-synth
;;;
;;; File
;;; rm-vid-synth.lsp
;;;
;;; Description
;;; A mode for rendering black and white video files: If any event is present
;;; in a pixel position, the pixel will be white, otherwise black.
;;;
;;; Output Format
;;; :video
;;;
;;; Required Slots
;;; start-time, duration,
;;; location, expansion
;;;
;;; Dependencies
;;; :zpng, :salza2
;;;
;;; Last Modified
;;; 2020/05/03
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-render-mode
 bin-vid-synth :video
 :required-slots
 (duration start-time location expansion)
 :required-packages (:zpng :salza2)
 :header-code
 (progn
   ;; ensure loc and expan have x and y values
   ;; if only one value is present, both are set to that value
   (loop for e in events do
        (location e (set-length 2 (location e)))
	(expansion e (set-length 2 (expansion e))))
   (let* (;; to save some time, we loop only once through the
	  ;; event list and temporarily save all values in vals
	  (vals
	   (loop for e in events
	      maximizing (+ (value (secs (start-time e)))
			    (value (secs (duration e))))
	      into total-dur
	      minimizing (- (value (first (location e)))
			    (* 1/2 (value (first (expansion e)))))
	      into min-x
	      maximizing (+ (value (first (location e)))
			    (* 1/2 (value (first (expansion e)))))
	      into max-x
	      minimizing (- (value (second (location e)))
			    (* 1/2 (value (second (expansion e)))))
	      into min-y
	      maximizing (+ (value (second (location e)))
			    (* 1/2 (value (second (expansion e)))))
	      into max-y
	      finally (return (list total-dur min-x max-x
				    min-y max-y))))
	  (total-dur (pop vals))
	  (min-x (pop vals))
	  (max-x (pop vals))
	  (min-y (pop vals))
	  (max-y (pop vals))
	  (frame-amount (ceiling (* 25 total-dur)))
	  (frame-array (make-array frame-amount :initial-element nil)))
     ;; fill frame-array with events
     (loop for frame below frame-amount
	for frame-st by 1/25 do
	  (loop for e in events do
	       (when (and (<= (value (secs (start-time e)))
			      frame-st)
		      	  (>= (+ (value (secs (start-time e)))
				 (value (secs (duration e))))
			      frame-st))
		 (push e (aref frame-array frame)))))
     ;; write a png per frame
     (loop for frame below frame-amount do
	  (let ((png
		 (make-instance 'zpng::pixel-streamed-png
				:width 1920 :height 1080
				:color-type :grayscale)))
	    (with-open-file
		(stream (ensure-directories-exist
			 (tmp-path (format nil "bin-vid-synth-~5,'0d.png" frame))) 
			:direction :output :if-exists :supersede
			:if-does-not-exist :create
			:element-type '(unsigned-byte 8))
	      (zpng::start-png png stream)
	      (loop for i below 1080 do
		   (loop for j below 1920 do
			(let ((white nil))
			  (loop for e in (aref frame-array frame) do
			       (when (and
			       	      (< (shift-value
					  (- (first (location e))
					     (* 1/2 (first (expansion e))))
					  min-x max-x 0 1920)
			       		 j)
			       	      (> (shift-value
					  (+ (first (location e))
					     (* 1/2 (first (expansion e))))
					  min-x max-x 0 1920)
			       		 j)
			       	      (< (shift-value
					  (- (second (location e))
					     (* 1/2 (second (expansion e))))
					  min-y max-y 0 1080)
			       		 i)
				      (> (shift-value
					  (+ (second (location e))
					     (* 1/2 (second (expansion e))))
					  min-y max-y 0 1080)
					 i))
				 (setq white t)))
			  (zpng::write-pixel
			   (if white '(255) '(0))
			   png))))
	      (zpng::finish-png png))))
     (run 'ffmpeg "-i" (tmp-path "bin-vid-synth-%05d.png")
	  "-c:v" "libx264"
	  "-vf" "fps=25,format=yuv420p"
	  (ensure-directories-exist
	   (absolute-path
	    (format nil 
		    "~a-bin-vid-synth.mp4" (name protagonist))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF

