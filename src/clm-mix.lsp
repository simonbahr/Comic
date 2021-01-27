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
;; cc/clm-mix.lsp                                                            2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cc)

;; Note: This file contains functions and instruments for rendering soundfiles
;; using clm. It will only be loaded if the clm-package is already present.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/mix(sound)
;;; Name
;;; mix
;;;
;;; File
;;; clm-mix.lisp
;;;
;;; Description
;;; This function is used to mix soundfiles after render with
;;; different render-modes for the same protagonist. However, it
;;; can also be used in various other situations to mix soundfiles.
;;;
;;; Files may have an arbitrary amount of channels.
;;;
;;; Note: This method is build upon clm and will not work and not
;;; even be loaded into the package when clm is not present. Always
;;; load clm before comic!
;;;
;;; Parameter
;;; output-type: Must be set to :sound to call this method.
;;; output-file: Path (with filename) to write the output-file to.
;;;              The output-file will be a .wav file. 
;;;
;;; Return Value
;;; boolean
;;;
;;; Example
;;; (comicp my-comic) --> t
;;;
;;; Last Modified
;;; 2019/12/21
;;;
;;; Synopsis
(defmethod mix ((output-type (eql :sound)) output-file
		files
		&key
		  (sample-rate 44100)
		  print
		  &allow-other-keys)
;;; ****
  ;; Check and prepare file-specs
  (let* ((channels 0)
	 (files
	  (loop for file in files collect
	       (let ((probe-sf (probe-file-with-suffix (value file) ".wav")))
		 (if probe-sf
		     (let ((chans (clm::mus-channels file)))
		       (when (> chans channels) (setq channels chans))
		       (namestring probe-sf))
		     (cc-error 'MIX-SOUND
			 "The file ~a does not exist or is not a wave-file.
                       You can only mix sound-files with .wav-file-extension."
		       file))))))
    (when print
      (format t "~&Mixing soundfiles..."))
    (when files
      (eval `(clm::with-sound
		 (:output
		  ,output-file
		  :srate ,sample-rate
		  :clipped nil
		  :scaled-to 1
		  :play nil
		  :channels ,channels
		  :header-type clm::mus-riff
		  :data-format clm::mus-lfloat)
	       ;; add channels/spacial positioning
	       ,@(loop for file in files collect
		      `(clm::cc-add-sound
			,file 0
			:channels ,channels))))))
  (when (probe-file output-file) t))

;; The following two functions are used with cc-samp5 to render a
;; soundfile according to its position on a protagonist in a
;; comic space.

(defun clm-get-channels (obj)
  "Returns the amount of channels needed to render
   a placed object to a soundfile"
  (let ((exp-obj (expansion obj)))
    (if (and (= (length exp-obj) 1)
	     (zerop (first exp-obj)))
	1 ;mono if no expansion
	;; else: 2^dimensions
	(expt 2 (length exp-obj)))))

;; NOTE: For 4-Channel-Output (or more) the mapping is
;;       Channel3=L and Channel4=R. Channels must be
;;       swapped according to specific needs ("ICEM 4-Kanal")
;; The function returns an array for easier use with clm
;; and to prevent quote-errors when calling with-sound
;; from the make-render-mode macro.
(defun clm-get-amp-scalers (event protagonist)
  "Returns amplitude scalers for any amount of dimensions.
   Event location and expansion must already be relative to 
   position on protagonist."
  (when (< (get-dimensions event) (get-dimensions protagonist))
    (set-dimensions event (get-dimensions protagonist)))
  (let* ((exp-obj (expansion protagonist))
	 (num-channels (clm-get-channels protagonist))
	 ;; exp and loc will range from 0 to 1 each:
	 (exp (expansion event))
	 (loc (loop for l in (location event) collect (* 1/2 (1+ l))))
	 (scalers '((1))));mono: two inputs will be distributed equally
    (unless (= 1 num-channels)
      (loop for ex in exp
	 for lo in loc
	 for limit below (length exp-obj)
	 do
	   (setq scalers
	   	 (append
	   	  (loop for scaler in scalers
	   	     collect
		       (append
			(loop for sc in scaler
			   collect
			     (clip (+ (* (abs (1- lo)) sc) (* 1/2 ex))))
			(loop for sc in scaler
			   collect
			     (clip (- (* (abs (1- lo)) sc) (* 1/2 ex))))))
	   	  (loop for scaler in scalers
		     collect
		       (append
			(loop for sc in scaler
			   collect
			     (clip (- (* lo sc) (* 1/2 ex))))
			(loop for sc in scaler
			   collect
			     (clip (+ (* lo sc) (* 1/2 ex))))))))))
    (make-array (list num-channels num-channels)
		:initial-contents scalers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
