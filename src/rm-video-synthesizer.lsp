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
;; render-modes/rm-vid-synth.lsp                                             2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

(defun frq2rgb (val &optional (gamma .8)) ; val in hz
  (let* (;; calc wavelength in nanometers
	 ;; (1m = 1000000000nm) 
	 (wlen (* 1000000000 (/ 299792458 val)))
	 (r 0)
	 (g 0)
	 (b 0))
    ;; this cond-block is based on
    ;; https://www.fourmilab.ch/documents/specrend/
    ;; and
    ;; http://www.physics.sfasu.edu/astro/color/spectra.html
    (cond ((and (>= wlen 380) (<= wlen 440))
	   (let ((attentuation
		  (+ .3 (/ (* .7 (- wlen 380))
			   (- 440 380)))))
	     (setq r (expt
		      (* (/ (* -1 (- wlen 440) (- 440 380)))
			 attentuation)
		      gamma))
	     (setq b (expt (* 1 attentuation) gamma))))
	  ((and (>= wlen 440) (<= wlen 490))
	   (setq g (expt
		    (/ (- wlen 440)
		       (- 490 440))
		    gamma))
	   (setq b 1))
	  ((and (>= wlen 490) (<= wlen 510))
	   (setq g 1)
	   (setq b (expt
		    (/ (* -1 (- wlen 510))
		       (- 510 490))
		    gamma)))
	  ((and (>= wlen 510) (<= wlen 580))
	   (setq r (expt
		    (/ (- wlen 510) (- 580 510))
		    gamma))
	   (setq g 1))
	  ((and (>= wlen 580) (<= wlen 645))
	   (setq r 1)
	   (setq g (expt
		    (/ (* -1 (- wlen 645))
		       (- 645 580))
		    gamma)))
	  ((and (>= wlen 645) (<= wlen 750))
	   (let ((attentuation
		  (+ .3 (/ (* .7 (- 750 wlen))
			   (- 750 645)))))
	     (setq r (expt attentuation gamma)))))
    ;;return rgb as list:
    (list (* r 255) (* g 255) (* b 255))))

#+zpng
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/video-synthesizer
;;; Name
;;; video-synthesizer
;;;
;;; File
;;; rm-vid-synth.lsp
;;;
;;; Description
;;; Transforms events to rectangular shapes in a videofile. If option
;;; pitch->color is t (default: nil), the color will be determined by the pitch
;;; slot, shifting values from the audible spectrum to the visible, if
;;; applicable.
;;;
;;; Not realy working yet!
;;;
;;; Output Format
;;; :video
;;;
;;; Required Slots
;;; start-time, duration,
;;; location, expansion
;;;
;;; Optional Slots
;;; pitch
;;;
;;; Dependencies
;;; :zpng, :salza2
;;;
;;; Last Modified
;;; 2020/02/17
;;;
;;; Synopsis
(make-render-mode
 video-synthesizer :video
 :required-slots
 (duration start-time location
	   expansion)
 :optional-slots (pitch)
 :options ((pitch->color nil))
 :required-packages (:zpng :salza2)
;;; ****
 :header-code
 (labels ((mix-rgb (rgbs)
	    (loop for (r g b) in rgbs
	       maximizing r into max-r
	       maximizing g into max-g
	       maximizing b into max-b
	       finally
		 (return (list (round max-r)
			       (round max-g)
			       (round max-b)))))
	  (shift-audib-visib (val) ; val in hz
	    (if (and (>= val 20)
		     (<= val 20000))
		(shift-value val 20 20000 ;min and max audio
			     400000000000000; min visible spectrum
			     789000000000000); max visible spectrum
		val)))
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
				:width 1920  :height 1080
				:color-type (if pitch->color
						:truecolor
						:grayscale))))
	    (with-open-file
		(stream (ensure-directories-exist
			 (tmp-path (format nil "vid-synth-~5,'0d.png" frame))) 
			:direction :output :if-exists :supersede
			:if-does-not-exist :create
			:element-type '(unsigned-byte 8))
	      (zpng::start-png png stream)
	      (loop for i below 1080 do
		   (loop for j below 1920 do
			(let ((pixel-evs))
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
				 (push e pixel-evs)))
			  (zpng::write-pixel
			   (if pitch->color
			       (mix-rgb
				(loop for e in pixel-evs
				   collect
				     (frq2rgb
				      (shift-audib-visib
				       (value (hz (pitch e)))))))
			       '(255))
			   png))))
	      (zpng::finish-png png))))
     (run 'ffmpeg "-i" (tmp-path "vid-synth-%05d.png")
	  "-c:v" "libx264"
	  "-vf" "fps=25,format=yuv420p"
	  (absolute-path
	   (format nil 
		   "~a-vid-synth.mp4" (name protagonist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
