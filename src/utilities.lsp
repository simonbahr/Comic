;c;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; utilities/utilities.lsp                                                   2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****ih* cc/utilities
;;; Name
;;; utilities
;;;
;;; File
;;; utilities.lsp
;;;
;;; Description
;;; - Functions for different purposes
;;; - Functions for generating various types of envelopes (= lists of numbers)
;;; - Definition of and functions for dealing with units.
;;;
;;; Classes
;;; unit, numeric-unit (as well as one subclass per implemented unit)
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/flat
;;; Name
;;; flat
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Flattens a list.
;;;
;;; Arguments
;;; ls: A list to flatten. Any other element will be returned
;;;     as a one-element list (also a flat list)
;;;
;;; Return Value
;;; always a flat list
;;;
;;; Example
#|
(flat '((1 2 3 (4)))) --> (1 2 3 4)
|#
;;;
;;; Last Modified
;;; 2019/07/23
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flat (ls)
  "takes a nested list or single element and always returns a flat list"
  (if (null ls)
      '()
      (if (listp ls)
	  (loop for elem in ls append
	       (cond ((null elem)
		      (list elem))
		     ((listp elem)
		      (append (flat (car elem)) (flat (cdr elem))))
		     (t (list elem))))
	  (list ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun flat? (ls)
  (if (listp ls)
      (cond ((null ls)
	     t)
	    ((listp (car ls))
	     nil)
	    (t (flat? (cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun single-cons-cell-p (ls)
  "Returns t if ls is a cons cell and cdr is not a list, eg '(1 . 2)."
  (when (and (consp ls) (not (listp (cdr ls)))) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/ensure-list
;;; Name
;;; ensure-list
;;;
;;; File
;;; utilities.lsp
;;;
;;; Description
;;; Will put the value in place into a list if it is not a list.
;;; The value will be changed in place!
;;;
;;; Arguments
;;; any element
;;;
;;; Return Value
;;; list
;;;
;;; Example
#|
(let ((val 1))
  (ensure-list val)
  val) --> '(1)

(let ((val '(1 2 3)))
  (ensure-list val)
  val) --> '(1 2 3)
|#

;;;
;;; Last Modified
;;; 2020/01/13
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ensure-list (place)
  `(if (listp ,place)
       ,place
       (setq ,place (list ,place))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-nth (list n val)
  "Set nth element of list to val and return the list"
  (if (>= n (length list))
      list
      (if (> n 0)
	  (cons (car list)
		(set-nth (cdr list) (1- n) val))
	  (cons val (cdr list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sum (ls)
  "Returns the sum of all elements in ls. 
   ls can be nested, but all elements must be numeric"
  (cond ((null ls) 0)
	((numberp ls) ls)
	((listp (car ls)) (+ (sum (car ls)) (sum (cdr ls))))
	((numberp (car ls)) (+ (car ls) (sum (cdr ls))))
	(t (cc-error 'sum "ls can only be a list of numbers and lists"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun each-nth (n ls)
  "returns each nth element in a list, starting with the first"
  (if (null ls)
      '()
      (cons (car ls) (each-nth n (nthcdr n ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun average (&rest ls)
  "Returns the average value of all parameters.
   Accepts numbers and lists of numbers (also nested)."
  (if (null ls)
      0
      (let ((list
	     (loop for elem in (flat ls) collect
		  (if (numberp elem)
		      elem
		      (cc-error 'average "list elements must be numeric")))))
	(/ (sum list) (length list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun median (list)
  "Returns the median of a list of numbers"
  (let* ((len (length list))
	 (ls (copy-seq list)) ;make non-destructive!
	 (nlst (sort ls #'>)))
    (if (evenp len)
        (/ (+ (nth (/ len 2) nlst)
              (nth (- (/ len 2) 1) nlst))
	   2)
        (nth (truncate (/ len 2)) nlst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun diversify-list (list)
  "resort a list so that the steps between numbers are as high as
   possible."
  (if (< (length list) 2)
      list
      (let ((new-second (loop for elem in (cdr list)
			      for n from 1
			      for diff = (abs (- elem (car list)))
			      with max-diff = 0
			      with nth = 1
			      when (> diff max-diff)
				do (progn (setq max-diff diff)
					  (setq nth n))
			      finally (return nth))))
	(cons (car list)
	      (cons (nth new-second list)
		    (diversify-list
		     (append
		      (subseq list 1 new-second)
		      (subseq list (1+ new-second)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pick-n (n list &key (force-unique t))
  "Picks n numbers from a list of numbers that (a) should not exceed the
   median 50% of the value range and (b) should be different as possible
   within that range."
  (if (>= n (length list))
      ;; simplest case: not enough values at all:
      list
      ;; else, find the 50%-median-range:
      (let* ((ls (copy-seq list))
	     (median (median ls))
	     (low-med (median
		       (loop for num in ls
			     when (<= num median)
			       collect num)))
	     (high-med (median
			(loop for num in ls
			      when (>= num median)
				collect num)))
	     (selection (loop for num in ls
			      when (<= low-med num high-med)
				collect num)))
	(if (and force-unique (> n (length selection)))
	    ;; if there are not enough numbers in 50% median range
	    ;; and force-n is t, select by distance to median:
	    (subseq
	     (sort ls (lambda (e1 e2)
			(when (< (abs (- e1 median))
				 (abs (- e2 median)))
			  t)))
	     0 n)
	    ;; else, select best matches by 
	    (sort (set-length n (diversify-list selection)) #'<)))))
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all? (proc ls)
  "Returns t if the proc returns true for all elements in ls."
  (if (apply proc (list (car ls)))
      (if (null (cdr ls))
	  t	     
	  (all? proc (cdr ls)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun all-equal? (list)
  "Returns t if all elements in ls and its sublists are equal."
  (setf list (flat list))
  (if (null (cdr list))
      t
      (if (equal (first list) (second list))
	  (all-equal? (cdr list))
	  nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/random-between
;;; Name
;;; random-between
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Returns a random number between min and max. 
;;; If :amount is >1, a list of random numbers is returned.
;;;
;;; Arguments
;;; min: minimum number
;;; max: maximum number
;;; no-float: only integers if t (default: nil)
;;; amount: amount of random numbers
;;;
;;; Return Value
;;; float, integer or list
;;;
;;; Example
#|
(random-between 0 1 :amount 1 :no-float nil)
|#
;;; Last Modified
;;; 2019/07/28
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun random-between (min max &key (no-float nil) (amount 1))
  "returns (a) random number(s) between min and max"
  (cond ((< 1 amount)
	 (loop repeat amount collect
	      (random-between min max
			      :no-float no-float :amount 1)))
	((equalp 1 amount)
	 (progn (if (> min max)
		    (setf min (prog1 max (setq max min))))
		(if (equalp min max)
		    min
		    (let ((random-arg
			   (if no-float
			       (abs (- max min))
			       (float (abs (- max min))))))
		      (+ min (random random-arg))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/clip
;;; Name
;;; clip
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Returns a value if it is between min and max. Else returns min
;;; or max. If val is a list, all numbers inside of it are clipped.
;;; If val is a string, it is clipped below char min and above char
;;; max. Anything else is simply returned.
;;;
;;; Arguments
;;; val: the value (number or list)
;;;
;;; Optional Arguments
;;; min: minimum number (default: 0)
;;; max: maximum number (default: 1)
;;;
;;; Return Value
;;; number or list
;;;
;;; Example
#|
(clip 5) --> 1
(clip 5 0 5) --> 5
(clip 10 0 5) --> 5
|#
;;;
;;; Last Modified
;;; 2019/08/04
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clip (val &optional (min 0) (max 1))
  "keep a value between min and max"
  (cond ((null val)
	 nil)
	((listp val)
	 (cons (clip (car val)) (clip (cdr val))))
	((stringp val)
	 (subseq val (floor min) (floor max)))
	((numberp val)
	 (if (< val min)
	     min
	     (if (> val max)
		 max
		 val)))
	(t val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun no-nil (list)
  "Returns a list without any nil-values, except at end of list."
  (cond ((null (car list))
	 (if (null (cdr list))
	     nil
	     (no-nil (cdr list))))
	((listp (car list))
	 (cons (no-nil (car list)) (no-nil (cdr list))))
	(t (cons (car list) (no-nil (cdr list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/relations
;;; Name
;;; relations
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Takes positive numbers and returns a list of numbers
;;; with equal relations to each other and a sum of 1 (= percentage)
;;;
;;; Rest Arguments
;;; values: positive numbers and lists of positive numbers (will be flattened)
;;;
;;; Return Value
;;; flat list
;;;
;;; Example
#|
(relations 1 9) --> (1/10 9/10)
|#
;;;
;;; Last Modified
;;; 2019/07/28
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun relations (&rest values)
  "forces the sum of values to be 1, preserving their relations"
  (setf values (flat values))
  (unless (all? (lambda (x) (> x 0)) values)
    (cc-error 'relations "all values must be positive"))
  (let ((sum-values (sum values)))
    (if (eq sum-values 0)
	nil
	(mapcar (lambda (x) (/ x sum-values)) values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-in-ls (ls)
  "Takes a flat list and returns a touple containing the maximum 
   value and its index"
  (unless (all? 'numberp ls)
    (cc-error 'max-in-ls
	"you must give a flat list of numbers as argument"))
  (labels ((min-in-ls-helper (ls min minindex index)
	     (if (null ls)
		 (list min minindex)
		 (if (> (car ls) min)
		     (min-in-ls-helper
		      (cdr ls) (car ls) index (1+ index))
		     (min-in-ls-helper
		      (cdr ls) min minindex (1+ index))))))
    (min-in-ls-helper ls 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-first-divisor (int)
  "finds the positive devisor of an integer that is closest to 0"
  (labels ((find-first-divisor-helper (int div)
	   (if (integerp (/ int div))
	       div
	       (find-first-divisor-helper int (1+ div)))))
    (if (integerp int)
	(cond ((equalp int 1)
	       1)
	      ((evenp int)
	       2)
	      (t (find-first-divisor-helper int 2)))
	(cc-error 'find-first-divisor "argument must be an integer"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/set-length
;;; Name
;;; set-length
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Takes a list or a single element an always returns a list of
;;; the specified length. If the original list is larger, it is cut.
;;; If the specified list is shorter, the list will be appended to
;;; itself until it matches the target-length.
;;;
;;; Arguments
;;; target-length
;;; ls
;;;
;;; Return Value
;;; list
;;;
;;; Example
#|
(set-length 8 '(1 2 3)) --> (1 2 3 1 2 3 1 2)
|#
;;;
;;; Last Modified
;;; 2019/07/28
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-length (target-length list)
  "Sets the length of a list by either cutting it or appending it
   to itself."
  (unless (or (<= target-length 0) (null list))
    (when (not (listp list))
      (setq list (list list)))
    (loop for n below target-length
	  collect
	  (nth (mod n (length list)) list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/stretch-list
;;; Name
;;; stretch-list
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Takes a list and stretches it to the specified length by either
;;; skipping or repeating elements.
;;;
;;; Arguments
;;; target-length
;;; list
;;;
;;; Return Value
;;; list
;;;
;;; Example
#|
(stretch-list 8 '(1 2 3)) --> (1 1 1 2 2 2 3 3)
|#
;;;
;;; Last Modified
;;; 2020/11/28
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stretch-list (target-length list)
  "Stretch a list to a specified length by skipping or repeating
   values." 
  (let ((len (length list))
	(tar target-length))
    (loop for n below tar
	  for nth = (floor (* (/ n tar) len))
	  collect (nth nth list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun circular-list (list)
  (setf (cdr (last list)) list)
  list)
;;; ensure that circular lists can be printed without entering an
;;; infinite loop
(setf *print-circle* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Seems to be inefficient! Running into debugger (heap exhausted)
;; with large strings.
(defun string-to-list (string &optional
				(delimiter " ")
				include-delimiter)
  "Turns a string into a list of strings, seperated by a delimiter."
  (if (stringp string)
      (let ((pos (search delimiter string)))
	(cond ((eq 0 (length string))
	       nil)
	      ((null pos)
	       (list string))
	      ((eq pos 0)
	       (string-to-list (subseq string (1+ pos))
			       delimiter
			       include-delimiter))
	      (t (cons (subseq string 0 (if include-delimiter
					    (1+ pos)
					    pos))
		       (string-to-list (subseq string (1+ pos))
				       delimiter
				       include-delimiter)))))
      (cc-error 'string-to-list "argument must be a string")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun only-last-occurance (list)
  "Removes all double occurances of an element from a list, only
   preserving the last one."
  (if (null list)
      nil
      (if (member (car list) (cdr list))
	  (only-last-occurance (cdr list))
	  (cons (car list) (only-last-occurance (cdr list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-unique (list)
  "counts all unique elements in a list"
  (length (only-last-occurance list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun only-first-occurance (list)
  "Removes all double occurances of an element from a list, only
   preserving the first one."
  (reverse (only-last-occurance (reverse list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ms-to-frame (ms &optional (fps 25))
  (if (>= ms 0)
      (1+ (floor (* fps (/ ms 1000))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun frame-to-ms (frame &optional (fps 25))
  (if (> frame 0)
      (* 1000 (/ (1- frame) fps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun max-length (&rest lists)
  "returns the length of the longest list"
  (loop for list in lists
     maximizing (length (flat list)) into max
     finally (return max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun merge-ls (&rest lists)
  "merges lists by iterating through the lists indexes
   [(1 2 3) (4 5) -> (1 4 2 5 3)]"
  (labels ((helper (lss result)
	     (if (null lss)
		 result
		 (if (null (car lss))
		     (helper (cdr lss) result)
		     (helper (append (cdr lss) (list (cdar lss)))
			     (if (null (caar lss))
				 result
				 (append result (list (caar lss)))))))))
    (helper lists nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun nil-stretch (target-length list)
  "stretches a list to target-length by filling it with
    nil-values (longer) or by linear interpolation (shorter)"
  (let ((len (length list))
	(tar target-length))
    (cond ((= tar len)
	   list)
	  ((< tar len)
	   (linpol target-length list))
	  ((> tar len)
	   (let* ((factor (/ tar len))
		  (index-value-touple
		   (loop for val in list
		      for i from 0 collect
			(list (truncate (* factor i)) val))))
	     (labels ((helper (i touples)
			(if (null touples)
			    nil
			    (if (= (caar touples) i)
				(cadar touples)
				(helper i (cdr touples))))))
	       (loop for i from 0 to (1- tar) collect
		    (helper i index-value-touple))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun get-bool (&optional (t-probability 1) (nil-probability 1))
;;   (if (not (equal nil-probability 1))
;;       (progn
;; 	(setf t-probability (* t-probability (/ 1 nil-probability)))
;; 	(setf nil-probability 1)))
;;   (if (<= nil-probability (* (+ t-probability nil-probability) (random 1.0)))
;;       t
;;       nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-keyword (symbol)
  "Turns any symbol into a keyword"
  (if (symbolp symbol)
      (values (intern (symbol-name symbol) "KEYWORD"))
      (cc-error 'make-keyword "NAME must be a symbol, not ~a."
	symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sublists-by-keywords (list &rest keywords)
  "Splits 'list' into sublists, which have any of the given keywords
   as car and everything until the next given keyword as cdr.
   If keywords is nil, every keyword will be considered.
   The function is not recursive, sublists will be treated as single
   elements."
  (loop for key in keywords do
       (unless (keywordp key)
	 (cc-error 'SUBLISTS-BY-KEYWORDS
	     "~a is not ~
            a keyword. ~%(list: ~g, keywords: ~g)"
	   key list keywords)))
  (let ((keywords-n))
    (loop for elem in list
       for n from 0 do
	 (when (or (member elem keywords)
		   (and (null keywords)
			(keywordp elem)))
	   (setq keywords-n (cons n keywords-n))))
    (setq keywords-n (append (reverse keywords-n)
			     (list (length list))))
    (append (subseq list 0 (car keywords-n))
	    (loop for start in keywords-n
	       for end in (cdr keywords-n)
	       collect
		 (subseq list start end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun type-or (thing types)
  "Check if a thing is of any of the given types"
  (loop for type in (flat types) do
       (when (typep thing type)
         (return t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun round-to-5 (number)
  "Rounds number to max 5 steps behind comma"
  (if (integerp number)
      number
      (* 0.00001 (round (* 100000 number)))))

(defun round-to-n (number n)
  "Rounds number to max 5 steps behind comma"
  (if (integerp number)
      number
      (let ((factor (expt 10.0 n)))
	(/ (round (* factor number)) factor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun todays-date (&optional destination)
  "get todays date as a string: 'day.month.year'"
  (multiple-value-bind
	(second minute hour date month year)
      (get-decoded-time)
    (declare (ignore second minute hour))
    (format destination "~d.~2,'0d.~d"
	    date month year)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/shift-value
;;; Name
;;; shift-value
;;;
;;; File
;;; utilities.lisp
;;;
;;; Description
;;; Shifts a value in a range between min and max to its proportional
;;; equivalent in the range between tar-min and tar-max
;;;
;;; Arguments
;;; (all numbers)
;;; val: The value
;;; min: The current minimum
;;; max: The current maximum
;;; tar-min: The target minimum
;;; tar-max: The target maximum
;;;
;;; Return Value
;;; number
;;;
;;; Example
#|
(shift-value .5 0 1 0 100) -> 50
|#
;;;
;;; Last Modified
;;; 2020/05/12
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shift-value (val min max tar-min tar-max)
  "Shift a value in a given range between min and max
   to a new range between tar-min and tar-max"
  (if (eq min max);otherwise division by 0
      (+ tar-min (- val min))
      (let (;; max*x+min=val?
	    (val-fac
	     (/ (- val min) (- max min))))
	;;apply this factor to new range
	(+ tar-min (* val-fac (- tar-max tar-min))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-power-of-2 (n) 
  (expt 2 (ceiling (/ (log n) (log 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun save-subseq (sequence start &optional end)
  "Subseq that will rather return nil than an error"
  (let ((len (length sequence)))
    (when (< start 0) (setq start 0))
    (when (and end (< end 0)) (setq end 0))
    (when (and end (> end len)) (setq end len))
    (when (and end (> start end)) (setq start end))
    (subseq sequence start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF utilities.lisp
