(in-package :clm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstrument cc-add-sound
    ;; A copy of the add-sound instrument from clm-5
    ;; used for mixing of soundfiles by protagonist.
    ;; Edited to mix n-Channels, not only 2.
    (file beg 
	  &key dur
	  channels
	  (orig-beg 0.0) 
	  (amp 1.0) 
	  (amp-env '(0 1 100 1))
	  (rev-amount .1))
  (let ((f (open-input* file :restartable t)))
    (unwind-protect
	 (let* ((st (floor (* beg *srate*)))
		(new-dur (or dur (- (sound-duration file) orig-beg)))
		(amp-val 0.0)
		(out-val 0.0)
		(orig-start (round (* orig-beg (sound-srate file))))
		(rd (make-array channels
				:initial-contents
				(loop for c below channels collect
				     (make-readin :file f :start orig-start :channel c))))
		(ampf (make-env :envelope amp-env :scaler amp :duration new-dur))
		(nd (+ st (floor (* *srate* new-dur)))))
	   (run
	    (loop for i from st to nd do
		 (setf amp-val (env ampf))
		 (loop for channel below channels do
		      (setf out-val (* amp-val (readin (aref rd channel))))
		      (out-any i out-val channel)
		      (if *reverb* (out-any i (* rev-amount out-val)
					    channel *reverb*))))))
      (close-input f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(definstrument cc-samp5
    ;; This is a modified version of the samp5-instrument
    ;; by Michael Edwards used in slippery-chicken.
    ;; It can produce an output-file with 2^n Channels,
    ;; depending on the dimensions of the protagonist.
    (file time out-channels &key
          (duration 0)
          (start 0)
          (end 0)
          (srt 1.0)
          (width 5)
          (srt-scaler 1.0)
	  ;; (srt-env '(0 0 100 0))
          (amp 1.0)
          amp-env
          (rev-amt 0)
          (snd-dur (sound-duration file))
          ;; default '#2A((1)) for mono output
	  (amp-scalers (make-array '(1 1) :initial-element 1)))
  (unless (zerop amp)
    (let* ((st (floor (* time *srate*)))
           (in-channels (mus-channels file))
           (input-dur (progn
                        (when (and (> end 0) (> start end))
                          (setq end snd-dur))
                        (when (> start end)
                          (setq start 0.0))
                        (if (zerop end)
                            (- snd-dur start)
                            (- end start))))
           (start-sample (floor (* *srate* start)))
           (f-in
	    (make-array out-channels
			:initial-contents
			(loop for c below out-channels collect
			     (open-input file :channel (mod c in-channels)
					 :start start-sample))))
           (max-out-dur (/ input-dur srt))
           (dur (max .2
		     (if (zerop duration)
			 max-out-dur
			 (min max-out-dur duration))))
	   (ampl-env
	    (if (and amp-env (> dur .4))
		amp-env
		(list 0 0 .015 1 (- dur .015) 1 dur 0)))
	   (gen-array
	    (make-array out-channels
			:initial-contents
			(loop for c below out-channels collect
			     (make-src :input (aref f-in c)
				       :srate srt :width width))))
           ;; (senv (make-env :envelope srt-env :scaler srt-scaler :offset 0.0 
           ;;                 :duration dur))
           (ampf (make-env :envelope ampl-env :scaler amp :duration dur))
	   (samps (make-array out-channels :initial-element 0.0))
	   (samp 0.0)
           (amp-val 0.0)
           ;; (sre-val 0.0)
	   (nd (+ st (floor (* *srate* dur))))
	   (fixed-amp-factor (/ 1 out-channels)))
      (run
       (loop for i from st to nd do
	    (setq ;; sre-val (env senv)
		  amp-val (env ampf))
	    (loop for c below out-channels do
		 (setf (aref samps c)
		       (src (aref gen-array c))))
	    (loop for channel below out-channels do
		 (setq samp
		       (loop for scaler below out-channels
		       	  summing (* (aref amp-scalers channel scaler)
		       		   amp-val
		       		   fixed-amp-factor
		       		   (aref samps scaler))))
		 (when *reverb* (out-any i
					 (* rev-amt samp)
					 channel
					 *reverb*))
		 (out-any i samp channel))))
      (loop for channel below in-channels do
      	   (close-input (aref f-in channel))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-wavesets (soundfile
		      &key
			from
			to
			(channel 0)
			(wavesets-per-event 1)
			max-event-amount)
  ;; get default values from soundfile object
  (when (typep soundfile 'soundfile)
    (unless from
      (setq from (value (start-time soundfile) 'secs)))
    (when (and (duration soundfile)
	       (not to))
      (setq to (value (u+ (start-time soundfile)
			  (duration soundfile))
		      'secs)))
    (setq soundfile (value soundfile)))
  ;; get defaults by ... defaults
  (unless from (setq from 0))
  (unless to (setq to (clm::sound-duration soundfile)))
  (let (;;get waveset-start-times with
	;;clm-ins "waveset-reader"
	(wavesets
	 (clm::waveset-reader soundfile
			      from to
			      channel
			      wavesets-per-event))
	(events nil))
    (unless max-event-amount
      (setq max-event-amount (length wavesets)))
    (loop for st in wavesets
       for next-st in (cdr wavesets)
       for n from 0
       do
	 (let ((dur (- next-st st)))
	   (unless (or (zerop dur)
		       (>= n max-event-amount))
	     (let ((ptch (/ 1 (/ dur wavesets-per-event))))
	       (push
		(make-event
		 :start-time (secs st)
		 :duration (secs dur)
		 :pitch (hz ptch)
		 ;; :amplitude -> maybe add amplitudes?
		 :soundfile (make-soundfile
			     soundfile
			     :start-time (secs st)
			     :duration (secs dur)
			     :pitch (hz ptch)))
		events)))))
    events))



(definstrument waveset-reader
    ;; Read zero-crossings from a soundfile
    ;; and return their positions in a list.
    ;; Used to read wavesets into events.
    (file
     &optional
     (beg 0)
     end
     (channel 0)
     (wavesets-per-event 1))
  (unless beg (setq beg 0))
  (unless end
    (setq end (sound-duration file)))
  (let*
      ;; the input file
      ((sfile (open-input file))
       ;; dur in secs
       (dur (- end beg))
       ;; start-time in samples
       (beg-in-samp (floor (* *srate* beg)))
       ;; end in samples
       (end-in-samp (floor (* *srate* end)))
       ;; duration in samples
       (dur-in-samp (floor (* *srate* dur)))
       ;; total amount of wavesets to collect
       (wavesets-amount 0)
       ;; total amount of zero-crossings
       (zero-crossings 0)
       (prev-samp 0.0)
       ;; array will hold start-sample-indexes of wavesets
       (array (make-array (ceiling (* 1/20 dur-in-samp))
			  :initial-element nil)))
    (run* (array wavesets-amount)
	  (loop for i from beg-in-samp below end-in-samp
	     do
	       (let ((the-samp (in-any i channel sfile)))
		 ;; set the index, if zero-crossing is found
		 (when (and (> the-samp 0)
			    (<= prev-samp 0)
			    (> i (1+ zero-crossings)))
		   (when (= 0 (mod zero-crossings
				   wavesets-per-event))
		     ;; set position of current sample
		     (setf (aref array wavesets-amount) (/ i *srate*))
		     ;; go to next array index
		     (incf wavesets-amount))
		   (incf zero-crossings))
		 ;; set the prev-samp value to current sample value
		 (setq prev-samp the-samp))))
    (loop for i below wavesets-amount collect
	 (aref array i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
