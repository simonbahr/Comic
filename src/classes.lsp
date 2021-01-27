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
;; main/classes.lsp                                                          2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;; macro for defining accessor functions with an optional value: if set, the slot
;; is set to that value and the object is returned. If not set, the current value
;; is returned.
(defmacro cc-make-accessor (slot-name class &optional value-types default-unit)
  ;; (setq slot-name (eval-when (:execute) slot-name)
  ;; 	class (eval-when (:execute) class)
  ;; 	value-types (eval-when (:execute) value-types))
  `(progn
     ;;GENERIC FUNCTION
     (defgeneric ,slot-name (obj &optional value))
     ;;METHOD FOR CLASS
     (defmethod ,slot-name ((obj ,class)
			    &optional
			      (value nil has-val))
       (if has-val
	   ;;set value:
	   (if (or (type-or value ,value-types) (null value))
	       (progn
		 (setf (slot-value obj
				   (quote ,slot-name))
		       (if (and (not (unit-p value))
			        ,default-unit)
			   (convert value ,default-unit)
			   value))
		 obj)
	       (cc-error (quote ,slot-name)
		   "The value ~a is not of any legal type ~
                  for ~a ~a." value (quote ,slot-name)
		  (quote ,value-types)))
	   ;;get value:
	   (slot-value obj
		       (quote ,slot-name))))
     ;;METHOD FOR LISTS: APPLY FOR EACH ELEMENT
     (defmethod ,slot-name ((obj list) &optional (value nil has-val))
       (if has-val
	   (if (listp value)
	       (loop for ob in obj for va in value do (,slot-name ob va))
	       (loop for ob in obj do (,slot-name ob value)))
	   (loop for ob in obj do (,slot-name ob))))
     ;;ERROR METHOD
     (defmethod ,slot-name
	 (obj &optional value)
       value
       (cc-error ',slot-name
	   "The object must be of type ~a, not ~a."
	      (quote ,class)
	      (type-of obj)))
     ;; SETF METHOD
     (defmethod (setf ,slot-name) (value (obj ,class))
       (,slot-name obj value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* main/get-slots
;;; Name
;;; get-slots
;;;
;;; File
;;; classes.lsp
;;;
;;; Description
;;; get-slots can be called with an instance of any class defined
;;; in Comic and returns a list of all slots this instance has.
;;;
;;; Parameter
;;; anything: any instance of a class defined by the comic-package
;;;
;;; Return Value
;;; list
;;;
;;; Last Modified
;;; 2020/06/22
;;;
;;; ****
(defgeneric get-slots (obj)
  (:method-combination append))

(defmethod get-slots append (obj) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* main/named-object
;;; Name
;;; named-object
;;;
;;; File
;;; classes.lsp
;;;
;;; Slots
;;; name
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
(defclass named-object ()
  ((name :initarg :name
	 :initform nil)))
;;; ****

(defmethod get-slots append ((obj named-object))
  '(name))

;; accessor for name:
(cc-make-accessor name named-object '(symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * * * * c * main/file
;;; Name
;;; file
;;;
;;; File
;;; classes.lsp
;;;
;;; Description
;;; The file class is a superclass of all classes with a
;;; that hold data about readable media-files on hard-disc.
;;;
;;; Slots
;;; path
;;;
;;; Last Modified
;;; 2020/05/13
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass file ()
  ((path :initarg :path
	 :initform nil)))

(defmethod initialize-instance :after ((class file) &key)
  "Check if the file exists. Will look in absolute path and 
   relative path to (cc-get :output-dir)"
  (let* ((path (slot-value class 'path))
	 (p1 (probe-file path))
	 (p2 (probe-file (make-pathname :directory
					(namestring (cc-get :output-dir))
					:name
					(namestring path)))))
    (cond (p1
	   (setf (slot-value class 'path) p1))
	  (p2
	   (setf (slot-value class 'path) p2))
	  (t (cc-error 'make-file
		 "The file ~a does not exist" path)))))

(defmethod get-slots append ((obj file))
  '(path))

(eval `(cc-make-accessor path file '(string pathname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object :before ((obj file) stream)
  (format stream "#<~a, PATH: ~a" (type-of obj) (path obj)))

(defmethod print-object ((obj file) stream))

(defmethod print-object :after ((obj file) stream)
  (format stream ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* main/info
;;; Name
;;; info
;;;
;;; File
;;; classes.lsp
;;;
;;; Description
;;; Prints info on any instance of a class defined in comic to
;;; std-out, including its slots and values, a structural overview
;;; of events and subecents (if applicable), and additional
;;; information on the objects status, etc.
;;;
;;; Parameter
;;; anything: any instance of a class defined by the comic-package
;;;
;;; Return Value
;;; t
;;;
;;; Last Modified
;;; 2020/01/08
;;;
;;; Synopsis
(defgeneric info (anything &key stream print-event-structure?))
;;; ****

(defmethod info :before ((anything named-object)
			 &key (stream t)
			   print-event-structure?)
  (declare (ignore print-event-structure?))
  (format stream "~&----~%~
INFO")
  (when (name anything)
    (format stream " ON ~a " (name anything)))
  (format stream " (Type: ~a)~%----~%"
	  (type-of anything)))


(defmethod info (anything &key (stream t) (print-event-structure? t))
  (let ((has-event-slot nil))
    (loop for slot in (get-slots anything) do
      (if (equal slot 'events)
	  (setq has-event-slot t)
	  (format stream "~&~a: ~a" slot (slot-value anything slot))))
    (when (and has-event-slot print-event-structure?)
      (format stream "~&----~%~
EVENT-STRUCTURE~%----~%")
      (print-events anything stream)))
  t)


;; IST DAS NICHT EHER EINE COMIC-FUNKTION?

;; NOT DONE YET:
;; - Symbole müssen quotiert werden (gibts schon in cca?)
;; - subevents müssen rekursiv gespeichert werden
;; - listen müssen als Listen lesbar sein, nicht evaluiert werden
;; - Strings brauchen ""


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF classes.lisp

