;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; event/event-generators.lsp                                                2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;; This file contains a collection of functions for the algorithmic
;;; creation of structures represented as lists of events with
;;; subevents, etc.

;;;NOTE WORKING YET:

;; (defun interpolate-event-sequences (event-seq1 event-seq2 &optional (steps 2))
;;   (let* ((seq-len1 (length event-seq1))
;; 	 (seq-len2 (length event-seq2))
;; 	 (seq-lens
;; 	   (mapcar #'round (line seq-len1 (list seq-len2 (1- steps)))))
;; 	 ;; (loop for n from seq-len1 to seq-len2
;; 	 ;; 	 by (/ (abs (- seq-len1 seq-len2)) steps)
;; 	 ;; 	 collect (round n)))
;; 	 (max-seq-len (apply #'max seq-lens))
;; 	 (interpolations
;; 	   (loop for e1 in (set-length max-seq-len event-seq1)
;; 		 for e2 in (set-length max-seq-len event-seq2)
;; 		 collect
;; 		 (interpolate-events e1 e2 steps)))
;; 	 (true-interpol-len (1+ (next-power-of-2 steps))))
;;     (print (length (car interpolations)))
;;     (loop 
;;       for len in seq-lens
;;       for n below steps
;;       collect
;;       (let ((nth-int (min
;; 		      (1- true-interpol-len)
;; 		      (+ n (floor
;; 			    (* (print (/ n steps))
;; 			       (print (- true-interpol-len n))))))))
;; 	(loop repeat len
;; 	      for int-ls in interpolations
;; 	      collect
;; 	      (progn
;; 		(format t "len: ~a, nth-int: ~a, n: ~a~%" len nth-int n)
;; 		(nth nth-int int-ls)))))))


;;e.g.: (comic-maker '((:duration 1) :duration 2 :pitch (hz 100))
;; (defun comic-maker (nested-args-list))



;; (defun palindrom-maker ()
;;   "Make a nice palindromic comic!"
;;   )

;; (defun make-events-from-weighted-values (amount &rest args)
;;   "Same key-args as make-event. Pass values like: '((100 1) (50 2)),
;;   with first elem beeing the slot-value and second elem the
;;   weight (amount of use). The function will try to generate as divers
;;   events as possible while keeing weighting the occurancies of each
;;   value. Exactly amount events will be returned."
;;  )
