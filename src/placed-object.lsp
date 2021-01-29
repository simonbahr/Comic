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
;; render/placed-object.lsp                                                  2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* placed-object/placed-object
;;; Name
;;; placed-object
;;;
;;; File
;;; placed-object.lsp
;;;
;;; Slots
;;; location, expansion, render-modes
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
(defclass placed-object ()
  ((location :initarg :location
	     :initform nil)
   (expansion :initarg :expansion
	      :initform nil)
   (render-modes :initarg :render-modes
		 :initform nil)
   (events :initarg :events
	   :initform nil)))
;;; ****

(defmethod initialize-instance :after ((obj placed-object) &rest initargs
				       &key &allow-other-keys)
  (declare (ignore initargs))
  (loop for slot in '(events render-modes location expansion)
     do
       (unless (listp (slot-value obj slot))
	 (setf (slot-value obj slot)
	       (list (slot-value obj slot))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; accessor functions for placed-objects:
(eval `(cc-make-accessor location placed-object
			 (quote ,(cc-get-event-slot-types 'location))))

(eval `(cc-make-accessor expansion placed-object
			 (quote ,(cc-get-event-slot-types 'expansion))))

(eval `(cc-make-accessor render-modes placed-object
			 (quote ,(cc-get-event-slot-types 'render-modes))))

(eval `(cc-make-accessor events placed-object
			 (quote ,(cc-get-event-slot-types 'events))))

(defmethod get-slots append ((obj placed-object))
  '(location expansion render-modes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* placed-object/get-dimensions
;;; Name
;;; get-dimensions
;;;
;;; File
;;; placed-object.lsp
;;;
;;; Description
;;; Returns the amount of dimensions a placed-object has.
;;; (= length of lists in location and expansion slots)
;;;
;;; Arguments
;;; obj: a placed-object
;;;
;;; Return Value
;;; integer
;;;
;;; Example
#|
(get-dimensions (event :location '(0 1 0))) --> 3
|#
;;;
;;; Last Modified
;;; 2020/02/14
;;;
;;; Synopsis
(defgeneric get-dimensions (obj))
;;; ****
(defmethod get-dimensions ((obj placed-object))
  (max (length (flat (location obj)))
       (length (flat (expansion obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* placed-object/set-dimensions
;;; Name
;;; set-dimensions
;;;
;;; File
;;; placed-object.lsp
;;;
;;; Description
;;; Sets the amount of dimensions of a placed-object.
;;; (= cuts the length of lists in location and expansion
;;; or fills it with 0s)
;;; If a value for location or expansion is nil,
;;; it will not be set!
;;;
;;; Arguments
;;; obj: a placed-object
;;;
;;; Return Value
;;; integer
;;;
;;; Example
#|
(set-dimensions (event :location '(0 1 0)) 2) --> location: '(0 1)
(set-dimensions (event :location '(0 1 0)) 4) --> location: '(0 1 0 0)
|#
;;;
;;; Last Modified
;;; 2020/02/14
;;;
;;; Synopsis
(defgeneric set-dimensions (obj dimensions))
;;; ****
(defmethod set-dimensions ((obj placed-object) dimensions)
  (let ((loc (location obj))
	(expan (expansion obj)))
    (when loc
      (location
       obj
       (set-length dimensions
		   (append (flat loc)
			   (set-length dimensions 0)))))
    (when expan
      (expansion
       obj
       (set-length dimensions
		   (append (flat expan)
			   (set-length dimensions 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* placed-object/get-distance
;;; Name
;;; get-distance
;;;
;;; File
;;; placed-object.lsp
;;;
;;; Description
;;; Calculates the (closest) distance between to placed-objects
;;; or coordinates.
;;;
;;; Arguments
;;; obj1, obj2 (both lists or numbers, or both placed objects)
;;;
;;; Return Value
;;; number
;;;
;;; Example
;;; (get-distance 0 1) -> 1.0
;;; (get-distance '(0 0) '(1 1)) -> 1.4142135
;;;
;;; Last Modified
;;; 2020/06/20
;;;
;;; Synopsis
(defgeneric get-distance (obj1 obj2))
;;; ****
(defmethod get-distance (pt1 pt2)
  "Calculates distance between two coordinates,
   like '(1 2) or '(1 1 1)."
  (unless (listp pt1)
    (setq pt1 (list pt1)))
  (unless (listp pt2)
    (setq pt2 (list pt2)))
  (loop for val1 in pt1
     for val2 in pt2
     summing (expt (- val1 val2) 2) into result
     finally (return (sqrt result))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod get-distance ((obj1 placed-object)
			 (obj2 placed-object))
  "Returns the closest distance between two placed objects."
  (let* ((loc1 (location obj1))
	 (loc2 (location obj2))
	 (exp1 (expansion obj1))
	 (exp2 (expansion obj2))
	 ;; coordinates of centers of objects:
	 (cen1 (mapcar (lambda (l e) (+ l (/ e 2))) loc1 exp1))
	 (cen2 (mapcar (lambda (l e) (+ l (/ e 2))) loc2 exp2)))
    (loop for l1 in loc1
       for l2 in loc2
       for e1 in exp1
       for e2 in exp2
       summing (let (;; the distance between the closest
		     ;; points on outer shell of both objects
		     (result (if (< l1 l2)
				 ;; (- (+ l1 (* .5 e1)) (- l2 (* .5 e2)))
				 ;; (- (+ l2 (* .5 e2)) (- l1 (* .5 e1)))))
				 (- (+ l1 e1) l2)
				 (- (+ l2 e2) l1 )))

		     ;; the distance between the centers of both
		     ;; objects
		     (center-dist (get-distance cen1 cen2)))
		 ;; the outer distance must be less or equal the
		 ;; distance between the centers. Otherwise, we
		 ;; have a "negative distance", meaning the objects
		 ;; overlap. In that case, the distance is 0.
		 (if (>= result center-dist)
		     (return 0)
		     ;; basic pythagoras:
		     (expt result 2)))
       into result
       finally (return (sqrt result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* placed-object/get-location-on-surface
;;; Name
;;; get-location-on-surface
;;;
;;; File
;;; placed-object.lsp
;;;
;;; Description
;;; Returns a new location for obj1, which is on the surface or
;;; in the body of obj2. This is a pretty dump implementation:
;;; It simply clips location values for each axis to fit the
;;; targeted range.
;;;
;;; The resulting location is adjusted to the dimensions of
;;; obj2, simply filling in coordinates of obj2 for dimensions
;;; not specified in obj1. The expansion of obj1 will not be
;;; taken into consideration.
;;;
;;; Arguments
;;; obj1, obj2: two placed object (e.g. events or protagonists)
;;;
;;; Return Value
;;; number or list
;;;
;;; Example
;;; (get-location-on-surface
;;;  (make-event :location '(0 0))
;;;  (make-protagonist :sound :location '(1 4) :expansion '(2 2)))
;;; -> (0 3.0)
;;;
;;; Last Modified
;;; 2020/06/20
;;;
;;; Synopsis
(defgeneric get-location-on-surface (obj1 obj2))
;;; ****
(defmethod get-location-on-surface ((obj1 placed-object)
				    (obj2 placed-object))
  "Returns a new location for obj1, 
   placing it on the surface (or in the body) of obj2.
   The expansion of obj1 will not be considered.
   This method is a rather rough approach and simply clips
   all location-values to the target range."
  (let ((loc1 (location obj1))
	(loc2 (location obj2))
	(exp2 (expansion obj2)))
    ;; go through obj2s axis, because the dimensions should
    ;; match obj2 rather than obj1. If obj1 is of lesser dimensions,
    ;; it will simply inherit obj2s values.
    (loop for l2 in loc2
       for e2 in exp2
       for n from 0 collect
	 (let ((l1 (nth n loc1))
	       (e (* .5 e2)))
	   (if l1
	       (clip l1 (- l2 e) (+ l2 e))
	       l2)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
