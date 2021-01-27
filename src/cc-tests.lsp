(in-package :comic)
;; module: cc

(cc-test cc-test
  (eq 1 1)
  (stringp "Hello World"))

(cc-test cc-setup
  (boundp '+cc-data+)
  (cc-get :src-dir)
  (listp (cc-get :event-slots)))

(cc-test utilites
  ;;flat
  (eq nil (flat '()))
  (equal '(1 2 3) (flat '(1 2 (3))))
  (equal '(1 2 3)
      (flat '((((((((((((((1 2))))) 3)))))))))))
  (equal '(1 2 3) (flat '((1) 2 (((((3))))))))
  (equal '(1) (flat 1))
  (equal '(1 2 3) (flat '(1 2 3)))
  (equal '(1 nil 2) (flat '(1 nil (2))))
  ;;flat?
  (flat? '())
  (flat? '(1 2 3))
  (not (flat? '(1 2 (3))))
  (not (flat? '(((1 ((2 3)))))))
  ;; sum
  (eq 0 (sum '()))
  (eq 1 (sum '(1)))
  (eq 6 (sum '(1 2 3)))
  (eq 0 (sum '(1 2 -3)))
  (eq 6 (sum '(((((((1 (((((2)))) 3))))))))))
  ;; each-nth
  (not (each-nth 1 '()))
  (equal '(1 3 5) (each-nth 2 '(1 2 3 4 5)))
  (equal '(1 4) (each-nth 3 '(1 2 3 4 5 6)))
  (equal '(1 2 3) (each-nth 1 '(1 2 3)))
  ;; average
  (eq 0 (average))
  (eq 1 (average  0 2))
  (eq 2 (average 1 '(2 3) 2))
  (eq 0 (average '((((((((1)))))))) '((2 (-3)))))
  ;; all?
  (all? 'numberp '(1 1 1 1))
  (all? 'listp '(()(())()))
  (not (all? 'numberp '(1 1 ())))
  (not (all? 'listp '(() "string" (())())))
  ;; all-equal?
  (all-equal? '(1 (1 1 1 1)))
  (all-equal? '(()(())()))
  (not (all-equal? '(2 (1 1 ()))))
  (not (all-equal? '("string" (1 1 ()))))
  ;; random-between
  (not (random-between 0 1 :amount 0))
  (eq 1 (random-between 1 1 :no-float t :amount 1))
  (equal '(1 1 1 1) (random-between 1 1 :no-float t :amount 4))
  (eq 0 (random-between 0 1 :no-float t))
  (<= 100 (random-between 100 1000))
  (>= 100 (random-between 0 100))
  ;; no-nil
  (eq NIL (no-nil NIL))
  (equal '(1 2) (no-nil '(1 NIL 2 NIL NIL)))
  (equal '(1 (2)) (no-nil '(1 NIL (2 NIL NIL))))
  ;; clip
  (equal "s" (clip "string"))
  (equal "str" (clip "string" 0 3))
  (eq 1 (clip 10))
  (eq 10 (clip 10 0 10))
  (eq 5 (clip 1 5 10))
  (eq -1.0 (clip -1.0 -10.0 10.0))
  (equal '(1 1 0.8 0 "s") (clip '(1 2 .8 -4 "string")))
  ;; relations
  (equal '(1/10 9/10) (relations 1 9))
  ;; max-in-ls
  (equal '(10 1) (max-in-ls (list 5 10 5 9)))
  ;; find-first-divisor
  (eq 1 (find-first-divisor 1))
  (eq 2 (find-first-divisor 10))
  (eq 17 (find-first-divisor 391))
  ;; set-length
  (not (set-length 0 '(1 2 3)))
  (not (set-length -2 '(1 2 3)))
  (equal '(1) (set-length 1 '(1 2 3)))
  (equal '(1 2 3) (set-length 3 '(1 2 3)))
  (equal '(1 2 3 1 2 3 1) (set-length 7 '(1 2 3)))
  (equal '(1 1 1 1 1) (set-length 5 1))
  (not (set-length 3 nil))
  ;; string-to-list
  (equal '("Hello" "World") (string-to-list "Hello World"))
  ;; only-last-occurance
  (equal '(3 2 1) (only-last-occurance '(1 2 3 2 1)))
  ;; count-unique
  (eq 0 (count-unique '()))
  (eq 5 (count-unique '(1 2 3 4 5)))
  (eq 5 (count-unique '(1 2 3 3 3 4 4 5 1 2 3 2 4 5 1)))
  ;; only-first-occurance
  (equal '(1 2 3) (only-first-occurance '(1 2 3 2 1)))
  ;; max-length
  (eq 3 (max-length '(1 2 3) '(4 5)))
  ;; merge-ls
  (equal (list 1 4 2 5 3)
	 (merge-ls '(1 2 3) '(4 5)))
  (equal (list 1 4 6 2 5 7 3 8 9 10 11)
	 (merge-ls '(1 2 3) '(4 5)
		   '(6 7 8 9 10 11)))
  (eq nil (merge-ls))
  (equal (list 1 2 3) (merge-ls '(1 2 3)))
  ;; nil-stretch
  (equal '(1 2 NIL 3 NIL)
	 (nil-stretch 5 '(1 2 3)))
  ;; make-keyword
  (eq :keyword (make-keyword 'keyword))
  (eq :keyword (make-keyword :keyword))
  ;; sublists-by-keywords
  (equal '((:ONE 1) (:TWO 2))
	 (sublists-by-keywords
	  '(:one 1 :two 2) :one :two))
  (equal '((:ONE 1 :TWO 2))
	 (sublists-by-keywords
	  '(:one 1 :two 2) :one))
  (equal '(:ONE 1 (:TWO 2))
	 (sublists-by-keywords
	  '(:one 1 :two 2) :two))
  (equal '((:ONE 1) (:TWO 2))
	 (sublists-by-keywords
	  '(:one 1 :two 2))))

(cc-test envelopes
  ;; env-maker
  (equal '(0 1/2 1 2/3 1/3 0)
	 (env-maker (lambda (x) x) 0 '((1 2) (0 3))))
  ;; line
  (equal '(0 1/3 2/3 1) (line  0 '(1 3)))
  (equal '(-1.0e10 9.9999995e20 1.9999999e21
	   2.9999997e21 3.9999998e21 5.0e21
	   5.9999994e21 6.9999995e21 7.9999996e21
	   8.9999997e21 1.0e22 0.0 0.47394595
	   0.9478919 1.4218378 1.8957838)
	 (line -10000000000
	       '(10000000000000000000000.0 10)
	       0 '(1.895783786496134 4)))
  ;; expo-n
  (equal '(0 1/9 4/9 1 34/25 61/25 106/25 169/25 10)
	 (expo-n 2 0 '(1 3) '(10 5)))
  (equal '(0 1/27 8/27 1 134/125 197/125 368/125
	   701/125 10 135/32 5/4 5/32 0)
	 (expo-n 3 0 '(1 3) '(10 5) '(0 4)))
  (equal '(-1.0e10 0.0 4.095e13 5.3144e15 1.6777214e17
	   2.4414062e18 2.1767823e19
	   1.38412874e20 6.871948e20 2.8242951e21 1.0e22
	   0.0 1.1299752e-7 4.6283784e-4
	   0.060051516 1.8957838)
	 (expo-n 12 -10000000000
		 '(10000000000000000000000.0 10) 0
		 '(1.895783786496134 4)))
  (equal '(-10.0 -9.999661 -9.65317 10.0)
	 (expo-n -10 -10 '(10.0 3)))
  ;; expo / expo2 / expo3
  (equal '(0 1/9 4/9 1 34/25 61/25 106/25 169/25 10
	   45/8 5/2 5/8 0)
	 (expo 0 '(1 3) '(10 5) '(0 4)))
  (equal ' (-1.0 -0.7777778 -0.111111104 1.0 8.0 9
		 2.625 0.5)
	  (expo2 -1 '(1.0 3) 8 9 '(0.5 2)))
  (equal '(0 1/27 8/27 1 134/125 197/125 368/125
	   701/125 10 135/32 5/4 5/32 0)
	 (expo3 0 '(1 3) '(10 5) '(0 4)))
  (equal '(-1.0 -0.9259259 -0.4074074 1.0 8.0 9
	   1.5625 0.5)
	 (expo3 -1 '(1.0 3) 8 9 '(0.5 2)))
  ;; loga-n / loga
  (equal '(1.0 6.6783676 10.0)
	 (loga-n 10 1 '(10 3)))
  (equal '(-10.0 -1.3864689 3.6521244 7.227062 10.0)
	 (loga-n 10 -10 '(10 5)))
  (equal '(1.0 6.6783676 10.0)
	 (loga 1 '(10 3)))
  (equal '(-10.0 -1.3864689 3.6521244 7.227062 10.0)
	 (loga -10 '(10 5)))
  ;; linpol
  (equal nil (linpol 1000 '()))
  (equal '(1 1 1 1) (linpol 4 '(1)))
  (equal '(1 1.75 2.5 3.25 4.0)
	 (linpol 5 '(1 2 3 4) :float t))
  (equal '(1 4.0 7.0 10.0 7.6666665 5.333333 3.0
	   -31.333336 -65.66667 -100.0)
	 (linpol 10 '(1 10 3 -100) :float t))
  ;; mix-envs
  (equal (list 1 4 6 7 2 5 8 3 9) (mix-envs '(1 2 3)
				       '(4 5) '( 6 7 8 9))))

(cc-test u-...-functions
  ;;u-... functions
  (u= 1 (hz 1))
  (u> 1 (hz .5))
  (u= .5 (u-mod (mins 2) (secs 45.0)))
  (unit-p (u+ 1 2 3 (secs 10)))
  (u= (mins 1) (u-round (secs 60.4)))
  (u= (midinote 66) (u1+ (midinote 65)))
  (let ((val (secs 1)))
    (u= (secs 0) (u1- val)))
  (u= (u-abs (secs -1)) 1)
  (u-zerop (hz 0))
  (u= 4.472136 (u-sqrt (hz 20)))
  (u-random (hz 1000)))

  
(cc-test comic-objects
  (find-class 'comic)
  (comicp (make-instance 'comic))
  (find-class 'event)
  (eventp (make-instance 'event))
  ;;...
  )

(cc-test events
  (doevents (e (loop-make-event
		  :duration from 10 to 12))
    (pitch e (hz 10)))
  (all? #'eventp
	(loop-make-event
	  :pitch from 0 to 3))
  ;;---
  )

(cc-test assign-ids-to-events
  ;; cc-assign-ids-to-events
  (equal
   '(0 2 4)
   (mapcar #'id
	   (cc-assign-ids-to-events
	    (loop-make-event :duration from 0 to 2
	       :events
		 (make-event)))))
  (equal
   '(0 3 6 9)
   (mapcar #'id
	   (cc-assign-ids-to-events
	    (loop-make-event :duration from 0 to 3
	       :events
		 (list (make-event)
		       (make-event)))))))

(cc-test comic
  (make-comic 'cc-test-comic
	      :author "Steve Jobs"
	      :render-modes 'cc-dummy-render-mode
	      :events
	      (append
	       (loop-make-event
		  :pitch 100
		  :duration 1
		  :amplitude from 0.1 to 1 by .1)
	       (list (make-event :name 'named-event
				 :events (make-event
					  :text
					  "I am a sub-sub-event")))))
  (eq 'named-event
      (name (get-event-by-name cc-test-comic 'named-event)))
  (= 1 (length (events (get-event-by-name cc-test-comic 'named-event)))))
