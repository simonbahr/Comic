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
;; render-modes/rm-videofile-mode.lsp                                        2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****R* render-mode/videofile-mode
;;; Name
;;; videofile-mode
;;;
;;; File
;;; rm-videofile-mode.lsp
;;;
;;; Description
;;; A basic render-mode for sampled video. It will place videofiles linked
;;; to in the videofile-slot of events in time. When a new event starts before
;;; the previous one is finished, the new one will replace the previous one,
;;; which will result in more activity overall. Frames will not be mixed!
;;;
;;; When pitch->speed is t (default: nil), the pitch-value of the event - if
;;; it is supplied - will result in a change of speed of the video: When the
;;; frame rate equals pitch in hz, the video will play back at its original
;;; speed. An octave up or down will result in double or half speed. (This can
;;; lead to potentially interesting effects when playing back the video with
;;; pitch values at audio rate, but starting from different frames
;;; each time.) If interpolate-frames is t (by default), frames are
;;; interpolated. If preserve-pitch is t (default: nil), the audio
;;; speed will be changed using time-stretch instead of samplerate-conversion.
;;;
;;; Another feature is the control of video brightness via the amplitude-slot.
;;; An amp of 1 will result in the original brightness, while an amp of 0 will
;;; result in a black screen. (Works in the opposite direction as well: amp 2
;;; results in a white screen.)
;;;
;;; If the video has an audio stream attached to it, the audio will be preserved
;;; in the result and cut the same way as the video. Any change of speed
;;; will be reflected in the audio, using ffmpegs "atempo".
;;;
;;; If there should be no events at some point, the gap will be filled
;;; using a monochrome color. This color can be set using the
;;; background-color option and defaults to black. (more info on
;;; syntax is here: https://ffmpeg.org/ffmpeg-utils.html#Color)
;;; 
;;; Output Format
;;; :video
;;;
;;; Required Slots
;;; videofile, start-time, duration
;;;
;;; Optional Slots
;;; pitch, amplitude
;;;
;;; Options
;;; pitch->speed (default: nil)
;;; interpolate-frames (default: t)
;;; preserve-pitch (default: nil)
;;; amplitude->brightness (default: nil)
;;; fps (frames per second, default: 25)
;;; height (default: 1080)
;;; width (default: 1920)
;;; background-color (default: black)
;;;
;;; Dependencies
;;; ffmpeg
;;;
;;; Last Modified
;;; 2020/06/18
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;BUG!: When first videofile starts after 0, acodec copy->no audio

;;;IDEA: Maybe include 'hq as value for interpolate-frames, find
;;;ffmpeg settings for veeeeery hq interpolation do use in final
;;;render. (mi_mode=mci:me_mode=bidir: ...)

;;;ADD: position/expansion using scaling and overlay filters
;;;     (maybe z axis too, with opacity in overlay?!)

;;;TRY: render lots of movs with alpha video (dur=st) and video. Then
;;;overlay all to final video in order of z-position. Would also fix
;;;timing issues, because all start-times were exact!

(make-render-mode videofile-mode
		  :video
		  :options
		  ((pitch->speed nil)
		   (interpolate-frames t)
		   (preserve-pitch nil)
		   (amplitude->brightness nil)
		   (fps 25)
		   (height 1080)
		   (width 1920)
		   (background-color 'black))
		  :required-slots
		  (videofile start-time duration)
		  :optional-slots
		  (pitch amplitude)
		  :required-software
		  (ffmpeg)
		  :header-code
		  (progn
		    ;; tmp1 will be used as time-counter
		    (setq tmp1 0)
		    ;; tmp2 will always holds start-times in secs
		    (setq tmp2
			  (loop for e in events collect
			       (value e 'secs 'start-time)))
		    ;; tmp3 will hold the output file path
		    ;; (setq tmp3 (absolute-path
		    ;; 		(format nil "~a-videofile-mode"
		    ;; 			(name protagonist))))
		    ;; tmp4 will be filled with paths to video-clips
		    (setq tmp4 nil))
		  :event-code
		  (let* ((st (pop tmp2))
			 (next-st (car tmp2))
			 (fill-dur (- st tmp1))
			 ;; ! By now, it is not checked how
			 ;; long the video output actually is.
			 ;; if a video duration is supplied
			 ;; that exceeds the input material,
			 ;; the times will not be rendered
			 ;; correctly...
			 (dur
			  (let ((vals
				 (list
				  (value duration 'secs)
				  (when next-st
				    (- next-st st))
				  (when (duration videofile)
				    (value (duration videofile) 'secs)))))
			    (apply #'min
				   (loop for val in vals when val collect val)))))
		    ;; generate an empty clip if needed in between
		    (when (>= fill-dur (/ 1 fps))
		      (let ((empty-clip (make-tmp-file nil "mp4")))
			(run 'ffmpeg
			     "-f" "lavfi" "-i"
			     (format nil "color=~a:s=~dx~d:r=~d/1"
				     background-color width height fps)
			     "-t"
			     (format nil "~f" fill-dur)
			     empty-clip)
			(incf tmp1 fill-dur)
			(push empty-clip tmp4)))
		    ;; render the subclip of the current video input
		    ;; ! ffmpeg crashes when rendering clips shorter 1 frame.
		    (when (>= dur (/ 1 fps))
		      (let* ((ffmpeg-args)
			     (input-ls (list
					"-i"
					(value videofile)
					"-ss"
					(format
					 nil "~f" 
					 (value (start-time videofile) 'secs))
					"-t"
					(format nil "~f" dur)))
			     (output-str (make-tmp-file nil "mp4"))
			     (frate-ls (list "-r" (format nil "~d" fps)))
			     ;; speed change is relative to frame-rate:
			     ;; with fps=25, a pitch of 50 hz results in double
			     ;; speed, etc.
			     (v-speed-factor
			      (if (and pitch->speed pitch)
				  (/ fps (value pitch 'hz))
				  1))
			     (interpolation-string
			      (if interpolate-frames
				  (format nil
					  "minterpolate='fps=~a:~
                                           mi_mode=blend',"
					  fps)
				  ""))
			     (a-speed-string
			      ;; FFMPEG can only change speed with
			      ;; factor 0.5–2. We need to call the
			      ;; filter multiple times:
			      (if preserve-pitch
				  (labels ((helper (n)
					     (cond
					       ((> n 2)
						(cons 2 (helper (/ n 2))))
					       ((< n 0.5)
						(cons 0.5 (helper (* n 2))))
					       (t (list n)))))
				    (format nil "~{atempo=~f~^,~}"
					    (helper (/ 1 v-speed-factor))))
				  (format nil "asetrate=48000*~a"
					  (/ 1 v-speed-factor))))
			     (brightness-factor
			      (if (and amplitude->brightness amplitude)
				  ;; amplitude 1 = no change in brightness.
				  ;; amplitude 0 = black screen
				  ;; (amplitude 2 = white screen)
				  (1- (value amplitude 'amp))
				  0))
			     (filter-ls
			      (unless (and
				       (= brightness-factor 0)
				       (= v-speed-factor 1))
				(list
				 "-filter_complex"
				 (format
				  nil
				  "[0:v]~
                                   scale=~dx~d,fps=fps=~d,setpts=~f*PTS,~a~
                                   eq=brightness=~f[v];[0:a]~a[a]"
				  width height fps v-speed-factor
				  interpolation-string brightness-factor
				  a-speed-string)
				 "-map" "[v]"
				 "-map" "[a]"
				 ;; prevents sync issues:
				 "-async" "1"))))
			(push output-str ffmpeg-args)
			(when filter-ls
			  (setq ffmpeg-args
				(append filter-ls ffmpeg-args)))
			(setq ffmpeg-args
			      (append input-ls frate-ls ffmpeg-args))
			;; create the file
			(apply #'run 'ffmpeg ffmpeg-args)
			;; increment current frame position
			(incf tmp1 dur)
			;; add the filename to list of files
			(push output-str tmp4))))
		  :footer-code
		  (let* ((concat-file
			  (make-tmp-file
			   (format nil "~{file ~d~^~%~}" (reverse tmp4))))
			 (output-file
			  (make-tmp-file
			   nil "mp4"
			   (format nil "~a-videofile-mode" (name protagonist)))))
		    ;; Concatenate files to final output file
		    (run 'ffmpeg "-f" "concat" "-safe" "0" "-i"
			 (format nil "~a" concat-file)
			 "-c" "copy" output-file)
		    ;; Return path if file was successfully created
		    (when (probe-file output-file)
		      (setq return-file-path output-file))))
