(in-package :clm)
;; Instrument for comic's sinewave-mode-mode
;; Generates a sine wave with given amplitude
;; and randomised phase (to prevent clicks
;; when synthesizing noise)
(definstrument additive-synthesis
    (freq amp dur st
	  &key (amp-scalers (make-array '(1 1) :initial-element 1))
	  (out-channels 1))
  (let* ((dur-in-samp (floor (* dur *srate*)))
	 (start-in-samp (floor (* st *srate*)))
         (attack (if (< dur .06)
                     (/ dur 10)
                     .006))
         (amp-env (make-env (list 0 0
				  attack 1
				  (- dur attack) 1
				  dur 0)
			    :duration dur))
	 (oscil (make-oscil freq (random PI))))
    (run
     (loop for c below out-channels do
	  (loop for i from start-in-samp below (+ start-in-samp dur-in-samp) do
	       (let ((out-samp (*
				(aref amp-scalers c)
				(oscil oscil) amp
				(env amp-env))))
		 (out-any i out-samp c)
	       ;; also write to reverb-channel
	       (when *reverb*
		 (out-any i out-samp c *reverb*))))))))
