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
;; utilities/units.lsp                                                       2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* unit/unit
;;; Name
;;; unit
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; A unit is an object with a value slot, holding a certain type of
;;; value. The objects type defines the way the value is interpreted.
;;; E.g.: <#100 Hz> is an object of type hz with a value of 100.
;;;
;;; Using unit-objects, values can be used in calculations directly,
;;; letting comic handle the conversion if possible. The functions
;;; are named u+, u- and so forth (see u-...). Comic does not check,
;;; whether the values given can be handled by these functions.
;;; The use of them is best suited for - but not restricted to -
;;; numeric-units.
;;;
;;; A unit object can and should be created using make-unit.
;;;
;;; Slots
;;; value
;;; base-unit
;;; value-type
;;; min-value
;;; max-value
;;;
;;; Last Modified
;;; 2020/02/03
;;;
;;; Synopsis
(defclass unit ()
  ((value
    :initarg :value
    :initform nil)
   (base-unit
    :initarg :base-unit
    :initform nil
    :reader base-unit
    :allocation :class)
   (value-type
    :initarg :value-type
    :initform nil
    :reader value-type
    :allocation :class)
   (min-value
    :initarg :min-value
    :initform nil
    :reader min-value
    :allocation :class)
   (max-value
    :initarg :max-value
    :initform nil
    :reader max-value
    :allocation :class)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* unit/numeric-unit
;;; Name
;;; numeric-unit
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; A unit is numeric if its value is a number. If no value is
;;; specified for a numeric-unit-object, it will default to 0.
;;; 
;;; Slots
;;; See superclass unit
;;;
;;; Last Modified
;;; 2020/02/03
;;;
;;; Synopsis
(defclass numeric-unit (unit)
  ()
  (:default-initargs :value 0))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default methods for printing units:
(defmethod print-object ((obj unit) stream)
  (format stream "#<~a: ~a" (type-of obj) (slot-value obj 'value)))

(defmethod print-object :after ((obj unit) stream)
  (loop for slot in (get-slots obj) do
       (unless (member slot '(value base-unit value-type
			      min-value max-value))
	 (format stream ", ~a: ~a" slot (slot-value obj slot))))
  (format stream ">"))

(defmethod print-object ((obj numeric-unit) stream)
  (format stream "#<~f ~a" (slot-value obj 'value) (type-of obj))
  ;; Readable printing of time-values:
  (when (typep obj '(or secs mins msecs hours))
    (let ((converted))
      (cond ((u< obj (msecs 1000))
	     (setq converted (msecs obj)))
	    ((u< obj (secs 60))
	     (setq converted (secs obj)))
	    ((u< obj (mins 60))
	     (setq converted (mins obj)))
	    (t
	     (setq converted (hours obj))))
      (unless (eq (type-of obj) (type-of converted))
	(when (slot-value converted 'value)
	  (format stream " (~f ~a)"
		  (slot-value converted 'value)
		  (type-of converted)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/unit-p
;;; Name
;;; unit-p
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Tests if an object is a unit.
;;;
;;; Arguments
;;; obj: The object
;;;
;;; Return Value
;;; boolean
;;;
;;; Example
#|
(unit-p (hz 100)) --> t
|#
;;;
;;; Last Modified
;;; 2020/02/03
;;;
;;; ****
(defun unit-p (obj)
  (typep obj 'unit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/convert
;;; Name
;;; convert
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Converts a unit from one type to another. Various methods
;;; are defined for different properties. In case a unit is not
;;; convertable to the specified type, an error is signaled.
;;;
;;; Arguments
;;; name: The object
;;;
;;; Return Value
;;; a unit-object
;;;
;;; Example
#|
(value 1) -> 1
(value (secs 1)) -> 1
(value (secs 1) 'mins) -> 1/60
(value (make-event :pitch (midinote 60)) 'hz 'pitch) -> 261.62555
|#
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defgeneric convert (obj unit))
;;; ****
(defmethod convert ((obj unit) unit)
  "returns prop if it is not converted and handles unconvertable types"
  (if (eq (type-of obj) unit)
      obj
      (error
       "IN PROP-CONVERT: A unit of type ~s can not be converted to ~s."
       (type-of obj) unit)))

(defmethod convert (obj unit)
  (if unit
      (funcall unit obj)
      obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* unit/value
;;; Name
;;; value
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Accesses the value slot of a unit-object.
;;; Can access values of events directly when unit-name is set
;;; (see event-method). Can convert value to another unit directly 
;;; when unit is set. For any non-unit, the input is simply returned.
;;;
;;; Arguments
;;; object: The object
;;; unit: unit to convert to. error is signaled if conversion fails.
;;; unit-name: The name of the unit to access. Only used
;;;                object is an event
;;;
;;; Example
#|
(value 1) -> 1
(value (secs 1)) -> 1
(value (secs 1) 'mins) -> 1/60
|#
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defgeneric value (object &optional unit slot-name))
;;; ****
(defmethod value ((object unit) &optional unit slot-name)
  (declare (ignore slot-name))
  (when unit
    (setq object (convert object unit)))
  (slot-value object 'value))

;; for any other data, value simply returns its input.
;; this way, (value (secs 1)) => 1 and (value 1) => 1.
(defmethod value (object &optional unit slot-name)
  (declare (ignore unit slot-name))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/make-unit
;;; Name
;;; make-unit
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Define a new unit. Creates a new class (subclass of either unit
;;; or numeric-unit, if the value-type is numeric), as well es a
;;; function for creating the unit and conversion- methods.
;;;
;;; Arguments
;;; name: The object
;;;
;;; Return Value
;;; any
;;;
;;; Example
#|
(value 1) -> 1
(value (secs 1)) -> 1
(value (secs 1) 'mins) -> 1/60
(value (make-event :pitch (midinote 60)) 'hz 'pitch) -> 261.62555
|#
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defmacro make-unit (name
		     value-type
		     &key
		       (function-name name)
		       direct-slots
		       make-proc-body
		       base-unit
		       conversion-to-base
		       conversion-from-base
		       min
		       max)
;;; ****
  (loop for expr in
       (list
	;;
	;; DEFINE CLASS
	;;
	`(defclass ,name
	     (,(if (eq value-type 'number) 'numeric-unit 'unit))
	   ,(if direct-slots
		(loop for slot in direct-slots collect
		     `(,slot :initarg ,(make-keyword slot) :initform nil))
		())
	   (:default-initargs :base-unit (quote ,base-unit)
	     :value-type (quote ,value-type)
	     :min-value (quote ,min)
	     :max-value (quote ,max)))
	;;
	;; DEFINE CREATION FUNCTION
	;;
	`(defun ,function-name ,(append
			'(value)
			(when direct-slots
			  '(&key))
			direct-slots)
	   (declare (ignorable ,@direct-slots))
	   ,(when (eq value-type 'number)
	      '(when (null value)
		(setq value 0)))
	   ;;check for type
	   (cond ((null value)
		  nil)
		 ((typep value (quote ,value-type))
		  ;;
		  ;; !!!
		  ;; min and max values were checked here.
		  ;; Problem: in calculations, values may be out of
		  ;; bounds but ok for render.
		  ;; TODO: -> ADD MIN-MAX Value Check before render!!!
		  ;;
		  ;; check for 
		  ;; ,(when max `(unless (<= value ,max)
		  ;; 		(cc-error (quote ,name)
		  ;; 		    "Value must not be greater than ~d"
		  ;; 		  ,max)))
		  ;; ,(when min `(unless (>= value ,min)
		  ;; 		(cc-error (quote ,name)
		  ;; 		    "Value must not be smaller than ~d"
		  ;; 		  ,min)))
		  ,(when make-proc-body make-proc-body)
		  (make-instance ',name
				 :value value
				 ,@(loop for slot in direct-slots
				      append
					(list (make-keyword slot) slot))))
		 ((unit-p value)
		  (convert value (quote ,name)))
		 (t
		  (cc-error (quote ,name)
		      "Value must be of type ~a"
		    (quote ,value-type)))))
	;;
	;; DEFINE CONVERSION METHODS
	;;
	;; convert from ,name to base unit:
	(when base-unit
	  `(defmethod convert
	       ((obj ,name) (unit (eql (quote ,base-unit))))
	     ,(if conversion-to-base
		  `(let ((value (value obj)))
		     (,base-unit ,conversion-to-base))
		  '(call-next-method))))
	;; from ,name anywhere else: first convert to base-unit
	(when base-unit
	  `(defmethod convert ((obj ,name) unit)
	     (if (eq unit (base-unit obj))
		 (call-next-method)
		 (convert (convert obj (quote ,base-unit)) unit))))
	;; from base-unit back to ,name
	(when (and base-unit conversion-from-base)
	  `(defmethod convert
	       ((obj ,base-unit) (unit (eql (quote ,name))))
	     ,(if conversion-to-base
		  `(let ((value (value obj)))
		     (,name ,conversion-from-base))
		  '(call-next-method))))
	;; from base-unit anywhere else: check if target unit has
	;; a base-unit. if not, call-next-method (error)
	(unless base-unit
	  `(defmethod convert ((obj ,name) unit)
	     (if (eq unit (base-unit obj))
		 (call-next-method)
		 (let ((unit-base (base-unit (make-instance unit))))
		   (if unit-base
		       (convert (convert obj unit-base) unit)
		       (call-next-method))))))
	`(defmethod get-slots append ((obj ,name))
		    ',(append
		       '(value base-unit value-type)
		       (if (eq value-type 'number)
			   '(min-value max-value)
			   '())
		       direct-slots)))
     do (eval expr))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNITS FOR TIME VALUES                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/secs
;;; Name
;;; secs
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in seconds.
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit secs number)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/microsecs
;;; Name
;;; microsecs
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in microeconds.
;;; Base unit: secs
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit microsecs number
	   :base-unit secs
	   :conversion-to-base (/ value 1000000)
	   :conversion-from-base (* value 1000000))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/msecs
;;; Name
;;; msecs
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in milliseconds.
;;; Base unit: secs
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit msecs number
	   :base-unit secs
	   :conversion-to-base (/ value 1000)
	   :conversion-from-base (* value 1000))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/mins
;;; Name
;;; mins
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in minutes.
;;; Base unit: secs
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit mins number
	   :base-unit secs
	   :conversion-to-base (* value 60)
	   :conversion-from-base (/ value 60))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/hours
;;; Name
;;; hours
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in hours.
;;; Base unit: secs
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit hours number
	   :base-unit secs
	   :conversion-to-base (* value 3600)
	   :conversion-from-base (/ value 3600))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/meter
;;; Name
;;; meter
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in meters.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit meter number)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/dmeter
;;; Name
;;; dmeter
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in deci-meters.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit dmeter number
	   :base-unit meter
	   :conversion-to-base (/ value 10)
	   :conversion-from-base (* value 10))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/cmeter
;;; Name
;;; cmeter
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in centi-meters.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit cmeter number
	   :base-unit meter
	   :conversion-to-base (/ value 100)
	   :conversion-from-base (* value 100))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/mmeter
;;; Name
;;; mmeter
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in milli-meters.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit mmeter number
	   :base-unit meter
	   :conversion-to-base (/ value 1000)
	   :conversion-from-base (* value 1000))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/yard
;;; Name
;;; yard
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in yards.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit yard number
	   :base-unit meter
	   :conversion-to-base (* value 0.9144)
	   :conversion-from-base (/ value 0.9144))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/inch
;;; Name
;;; inch
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a distance in inches.
;;;
;;; Last Modified
;;; 2021/01/29
;;;
;;; Synopsis
(make-unit inch number
	   :base-unit meter
	   :conversion-to-base (* value 0.0254)
	   :conversion-from-base (/ value 0.0254))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/beats
;;; Name
;;; beats
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a time value in beats.
;;; Base unit: secs
;;; If no tempo is set, it will default to 60.
;;;
;;; Slots
;;; tempo
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit beats number
	   :base-unit secs
	   :direct-slots (tempo)
	   :conversion-to-base
	   (/ (* value 60)
	      (slot-value obj 'tempo))
	   :conversion-from-base value
;;; ****
	   :make-proc-body (unless tempo (setq tempo 60)))

(make-unit samples number
	   :base-unit secs
	   :direct-slots (sample-rate)
	   :conversion-to-base
	   (/ value (slot-value obj 'sample-rate))
	   :conversion-from-base
	   (* value (slot-value obj 'sample-rate)))

(make-unit frames number
	   :base-unit secs
	   :direct-slots (frame-rate)
	   :conversion-to-base
	   (/ value (slot-value obj 'frame-rate))
	   :conversion-from-base
	   (* value (slot-value obj 'frame-rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNITS FOR PITCH VALUES                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/hz
;;; Name
;;; hz
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a frequency value in hz.
;;; Base unit: secs
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit hz number :min 0
	   :base-unit secs
	   :conversion-to-base (if (zerop value) 0 (/ 1 value))
	   :conversion-from-base (if (zerop value) 0 (/ 1 value)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/midinote
;;; Name
;;; midinote
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a frequency value as midinote.
;;; Base unit: hz
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit midinote number :min 0 :max 127
	   :base-unit hz
	   :conversion-to-base (if (zerop value)
				   0
				   (* 440 (expt 2 (/ (- value 69) 12))))
	   :conversion-from-base (if (zerop value)
				     0
				     (+ 69 (* 12 (log (/ value 440) 2)))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/khz
;;; Name
;;; khz
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a frequency value in khz.
;;; Base unit: hz
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit khz number :min 0
	   :base-unit hz
	   :conversion-to-base (* 1000 value)
	   :conversion-from-base (/ value 1000))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-unit mhz number :min 0
	   :base-unit hz
	   :conversion-to-base (* (expt 10 6) value)
	   :conversion-from-base (* (expt 10 -6) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-unit ghz number :min 0
	   :base-unit hz
	   :conversion-to-base (* (expt 10 9) value)
	   :conversion-from-base (* (expt 10 -9) value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/thz
;;; Name
;;; thz
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a frequency value in thz.
;;; Base unit: hz
;;;
;;; What? Terahertz?
;;; This unit is added to Comic to represent different colors
;;; in the visible spectrum of light.
;;; 
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit thz number :min 0
	   :base-unit hz
	   :conversion-to-base (* (expt 10 12) value)
	   :conversion-from-base (* (expt 10 -12) value))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNITS FOR DYNAMIC VALUES                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/amp
;;; Name
;;; amp
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing an amplitude.
;;; It can e.g. be seen as the relative amplitude of an audio signal.
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit amp number :min 0 :max 1)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/dbfs
;;; Name
;;; dbfs
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing an amplitude in db-fullscale.
;;; The maximum amplitude is 0, the minimum amplitude is -infinity.
;;; Base unit: amp
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit dbfs number :min -18446744073709551616 :max 0
	   :base-unit amp
	   :conversion-to-base (expt 10 (/ value 20))
	   :conversion-from-base (if (zerop value)
				     -18446744073709551616
				     (* 20 (log value 10))))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/midivelocity
;;; Name
;;; midivelocity
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing an amplitude as a midivelocity.
;;; Base unit: amp
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit midivelocity number :min 0 :max 127
	   :base-unit amp
	   :conversion-to-base (/ value 127)
	   :conversion-from-base (* value 127))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/dynamic-symbol
;;; Name
;;; amp
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Unit representing a dynamic value as a symbol (piano, forte, 
;;; etc.). It is in some cases possible to use this unit in
;;; calculations, as conversions to midivelocities are implemented.
;;; But be careful: The conversion has to happen before the
;;; calculation, as any procedure like u+, etc. will signal an error
;;; when called with a symbol.
;;;
;;; Base unit: midivelocity
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit dynamic-symbol symbol
	   :base-unit midivelocity
	   :conversion-to-base (case value
				 (fff 127)
				 (ff 112)
				 (f 96)
				 (mf 80)
				 (mp 64)
				 (p 48)
				 (pp 32)
				 (ppp 16))
	   :conversion-from-base (cond
				   ((> value 112) 'fff)
				   ((> value 96) 'ff)
				   ((> value 80) 'f)
				   ((> value 64) 'mf)
				   ((> value 48) 'mp)
				   ((> value 32) 'p)
				   ((> value 16) 'pp)
				   ((> value 0) 'ppp)
				   ((<= value 0) nil))
;;; ****
	   :make-proc-body
	   (unless (member value '(fff ff f mf mp p pp ppp))
	     (cc-error 'DYNAMIC-SYMBOL
		 "~a is not a dynamic-symbol.~%~
                         Legal Values are fff, ff, f, mf, mp, p, ~
                         pp, and ppp." value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UNITS FOR TEXT/LANGUAGE REPRESENTATION                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/xsampa
;;; Name
;;; xsampa
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Unit representing spoken language in the x-sampa-format.
;;; Conversion from any string to x-sampa is implemented, although
;;; pretty rough: May be enough to produce language-like sounds
;;; with a voice-synthesis-program, but not enough to produce
;;; understandable spoken language in any case. The main point here
;;; is to exclude all characters that do NOT belong to the x-sampa-
;;; system.
;;;
;;; A list of legal characters is available here:
;;; https://en.wikipedia.org/wiki/X-SAMPA
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit xsampa string
;;; ****
	   :make-proc-body
	   (let ((char-ls
		  (loop for n below (length value) collect
		       (let ((char (subseq value n (1+ n))))
			 (cond
			   ((search char
				    "aeEioOuywjHvzZfsSbdgptkmnNRl")
			    char)
			   ((search char
				    "IUYWJVFBDGPTKML")
			    (string-downcase char))
			   ((equal char "A") "a~~")
			   ((equal char "h") "H")
			   ((equal char "r") "R")
			   ((search char "yY") "i")
			   ((search char "cCqQxX") "k")
			   (t "_"))))))
	     (setq char-ls (loop for char in char-ls append
				(list char " ")))
	     (setq value (apply #'concatenate 'string char-ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OBJECT FOR REPRESENTATION OF AUDIO/VIDEO MATERIAL            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/soundfile
;;; Name
;;; soundfile
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Unit representing a soundfile. The value is the path to the
;;; file.
;;;
;;; Slots
;;; start-time: The start-time of the desired excerpt (default: 0).
;;; duration: The duration of the desired excerpt (if applicable,
;;; may default to the entire duration when the clm-package can be
;;; used to detect it).
;;; pitch: The pitch of the soundfile (if applicable).
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit soundfile string
	   :function-name make-soundfile
	   :direct-slots
	   (start-time duration pitch)
;;; ****
	   :make-proc-body
	   (progn
	     ;; check if file exists and is a wav-file:
	     (unless (probe-file value)
	       (cc-error 'SOUNDFILE
		   "The file ~a does not exist."
		 value))
	     (unless
		 (and (> (length value) 4)
		      (string-equal
		       value ".wav"
		       :start1 (- (length value) 4)))
	       (cc-error 'SOUNDFILE
		   "File extension not recognised. ~
                        Please choose a .wav-file."))
	     ;; set default values:
	     (unless start-time
	       (setq start-time (secs 0)))
	     (unless duration
	       (setq duration
		     (u- (secs (get-wav-file-data value))
			 start-time)))
	     ;; using Michaels get-spectrum function
	     ;; to find the most prominent partial
	     ;; at start-time of soundfile and use it
	     ;; as pitch value.
	     ;; This is pretty dumb, but probably
	     ;; effective in some situations...
	     #+sc
	     (unless pitch
	       (setq pitch
		     (hz
		      (first ;get-sprectrum always returns a list
		       (clm::get-spectrum
			value
			:num-partials 1
			:start-analysis
			(value start-time 'secs))))))))

(cc-make-accessor start-time soundfile '(numeric-unit number) 'secs)
(cc-make-accessor duration soundfile '(numeric-unit number) 'secs)
(cc-make-accessor pitch soundfile '(numeric-unit number) 'hz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/videofile
;;; Name
;;; videofile
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Unit representing a videofile. The value is the path to the
;;; file.
;;;
;;; Slots
;;; start-time: The start-time of the desired excerpt (default: 0).
;;; duration: The duration of the desired excerpt (if applicable).
;;; frate: frame-rate of the videofile.
;;; height / width: pixel dimensions of the videofile.
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit videofile string
	   :function-name make-videofile
	   :direct-slots
	   (start-time duration frate height width)
;;; ****
	   :make-proc-body
	   (progn
	     (unless (probe-file value)
	       (cc-error 'VIDEOFILE
			 "The file ~a does not exist."
			 value))
	     (unless frate (setq frate 25)) ; used at all?!
	     (unless height (setq height 1080))
	     (unless width (setq width 1920))
	     (unless start-time (setq start-time 0))
	     ;; parsing video-duration via ffmpeg
	     #+ffmpeg
	     (when (not duration)
	       (let* ((dur-string
			(second
			 (member "Duration:"
				 (string-to-list
				  (run-and-return-output
				   'ffmpeg "-i" value))
				 :test #'string-equal)))
		      (parsed-duration
			(u+
			 (secs (parse-integer dur-string
					      :start 6
					      :end 8))
			 (hours (parse-integer dur-string
					       :start 0
					       :end 2))
			 (mins (parse-integer dur-string
					      :start 3
					      :end 5))
			 (msecs (* 10 (parse-integer dur-string
						     :start 9
						     :end 11))))))
		 (setq duration
		       (u- parsed-duration start-time))))))

(cc-make-accessor start-time videofile '(numeric-unit number) 'secs)
(cc-make-accessor duration videofile '(numeric-unit number) 'secs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RELATIVE UNIT                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****U* unit/%
;;; Name
;;; %
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Numeric unit representing a percentage-value.
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(make-unit % number)
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/%->
;;; Name
;;; %->
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Takes a value, a range and a unit and outputs the value as a
;;; plain number, converted to the unit. The value may be an instance
;;; of the %-unit-class, in which case the absolute value will
;;; be determined by the given range, another kind of unit, given it
;;; can be converted to the target-unit, or a plain number, in which
;;; case it will be interpreted as a percentage-value.
;;;
;;; Arguments
;;; value: A unit object convertable to unit (may be a %-object)
;;;        or a number, interpreted as %-value
;;; min: the minimum of the target-range
;;; max: the maximum of the target-range
;;; unit: the unit to convert to.
;;;
;;; Optional Arguments
;;; float: Indicates whether to always return a float (default: t).
;;; round: Indicates whether to round the result to an integer
;;;        (default: nil).
;;;
;;; Return Value
;;; number
;;;
;;; Example
#|
(%-> 1 0 1 'secs) --> 0.01
(%-> (secs 1) 0 1 'secs) --> 1.0
|#
;;;
;;; Last Modified
;;; 2020/06/24  
;;;
;;; Synopsis
(defun %-> (value min max unit
	    &optional (float t) (round nil))
;;; ****
  (cond ((typep value '%)
	 (%->
	  (funcall unit
		   (+ min (* (/ (value value) 100) (- max min))))
	  min max unit))
	((numberp value)
	 (%-> (% value) min max unit))
	((typep value unit)
	 (cond (round
		(round (value value)))
	       (float
		(float (value value)))
	       (t (value value))))
	((unit-p value)
	 (%-> (convert value unit) min max unit))
	((null value) min)
	(t
	 (cc-type-error '%-> value '(or value number unit)))))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/apply-with-unit
;;; Name
;;; apply-with-unit
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Calls proc with an argument list that may include properties.
;;; The returned type equals the type of the first propertie in args.
;;;
;;; Arguments
;;; proc: A function
;;; args: A list of arguments to proc
;;;
;;; Return Value
;;; any
;;;
;;; Example
#|
(apply-with-unit #'+ (list (midinote 69) 1)) --> [70 midinote]
|#
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defun apply-with-unit (proc args)
  "Calls proc with an argument list that may include properties.
   The returned type equals the type of the first unit in args."
;;; ****
  (let ((result 0)
	(type nil))
    (setq
     args
     (loop for arg in args collect
	  (cond ((and (unit-p arg) (not type))
		 (setq type (type-of arg))
		 (value arg))
		((unit-p arg)
		 (value (convert arg type)))
		(t
		 arg))))
    (setq result (apply proc args))
    (if (and type
	     (not (typep result 'boolean))
	     (typep result (value-type (make-instance type))))
	(convert result type)
	result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/funcall-with-unit
;;; Name
;;; funcall-with-unit
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; See documentation of unit/apply-with-unit for more info.
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defun funcall-with-unit (function &rest arguments)
;;; ****
  (apply-with-unit function arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* unit/u-...
;;; Name
;;; u-... (functions that can handle units)
;;;
;;; File
;;; unit.lsp
;;;
;;; Description
;;; Comic provides a collection of function that are derived from
;;; ANSII-CL equivalents and are able to take unit-objects as
;;; arguments. The standard collection focusses on - but is not
;;; limited to - arithmetic operations. If needed, additional
;;; u-functions can be easily defined using cc-make-u-function.
;;; You can also use u-funcall or u-apply to run
;;; standard-procedures with unit-objects as arguments.
;;;
;;; Some standard u-functions are:
;;; u+, u-, u*, u/, u-mod, u-round, u-ceiling, u-floor, u=, u>, u<
;;; u>=, u<=, u-eq, u-equal, u1+, u1-, u-abs, u-exp, u-expt, u-log,
;;; u-float, u-zerop, u-sqrt
;;; (See Synopsis for an up-to-date list)
;;;
;;; Example
#|
(with-unit (> (mins 3) (secs 2))) --> T
(with-unit (concatenate 'string "Hello " "World")) --> "Hello World"
N.B.: (with-unit (= (mins 1) 1)) --> T
      Values with units can be compared with plain numbers. 
      Handle with care...
|#
;;; 
;;;
;;; Last Modified
;;; 2020/05/06
;;;
;;; Synopsis
(defmacro cc-make-u-function (u-name function-name)
  `(defun ,u-name (&rest args)
     (apply-with-unit #',function-name args)))

(cc-make-u-function u+ +)
(cc-make-u-function u- -)
(cc-make-u-function u* *)
(cc-make-u-function u/ /)
(cc-make-u-function u-mod mod)
(cc-make-u-function u-round round)
(cc-make-u-function u-ceiling ceiling)
(cc-make-u-function u-floor floor)
(cc-make-u-function u-min min)
(cc-make-u-function u-max max)
(cc-make-u-function u= =)
(cc-make-u-function u> >)
(cc-make-u-function u< <)
(cc-make-u-function u>= >=)
(cc-make-u-function u<= <=)
(cc-make-u-function u-eq eq)
(cc-make-u-function u-equal equal)
(cc-make-u-function u1+ 1+)
(cc-make-u-function u1- 1-)
(cc-make-u-function u-abs abs)
(cc-make-u-function u-exp exp)
(cc-make-u-function u-expt expt)
(cc-make-u-function u-log log)
(cc-make-u-function u-float float)
(cc-make-u-function u-zerop zerop)
(cc-make-u-function u-sqrt sqrt)
(cc-make-u-function u-sin sin)
(cc-make-u-function u-cos cos)
(cc-make-u-function u-random random)
;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF comic/properties.lisp
