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
;; render/render-mode.lsp                                                    2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/render-mode
;;; Name
;;; render-mode
;;;
;;; File
;;; render-mode.lsp
;;;
;;; Description
;;; A render-mode is an object that stores code to execute during
;;; render. It may be configured by options stored in its option-slot
;;; It may be build upon or make use of other lisp-packages or
;;; external-software. These should be specified in the corresponding
;;; slots, so Comic is able to check if a mode can be used.
;;;
;;; A render-mode must always be created using the macro
;;; make-render-mode. It stores the created mode in +cc-data+, which
;;; makes it easier to use it later. And, most importantly, it also
;;; instanciates a specific method render-events for each render-mode,
;;; which is called by the render-function.
;;;
;;; Slots
;;; output-type, options, required-slots, optional-slots,
;;; required-packages, required-software, header-code,
;;; before-code, event-code, after-code, footer-code,
;;; group-events
;;;
;;; Last Modified
;;; 2020/06/23
;;;
;;; Synopsis
(defclass render-mode (named-object)
  ((output-type
    :accessor output-type
    :initarg :output-type
    :initform nil)
   ;; options is a list of local variables that will be
   ;; accessable in the render-code and can be set dynamically
   ;; before each render. Must be a list of lists! (let-bindings)
   (options
    :accessor options
    :initarg :options  
    :initform nil)
   (required-slots
    :initarg :required-slots
    :initform nil)
   (optional-slots
    :initarg :optional-slots
    :initform nil)
   ;; is mode dependent on other lisp-packages?
   ;; (nil or list)
   (required-packages
    :initarg :required-packages
    :initform nil)
   ;; is mode dependent on external software?
   ;; (nil or list)
   (required-software
    :initarg :required-software
    :initform nil)
   ;; code to execute at start of render
   ;; of events to one protagonist
   (header-code
    :initarg :header-code
    :initform nil)
   ;; code to execute before event-code.
   ;; when events are grouped, it is 
   ;; executed only once per group
   ;; (meaning: event will be a list!)
   (before-code
    :initarg :before-code
    :initform nil)
   ;; code to execute once per event
   ;; unit-values are bound to their
   ;; names automatically
   (event-code
    :initarg :event-code
    :initform nil)
   ;; code to execute after event-code
   ;; when events are grouped, it is 
   ;; executed only once per group
   (after-code
    :initarg :after-code
    :initform nil)
   ;; code to execute at end of render
   (footer-code
    :initarg :footer-code
    :initform nil)
   ;; if t, call group-events and execute
   ;; before- and after-code before and
   ;; after each group.
   ;; events are grouped if they follow
   ;; each other directly in the list
   ;; of events as well as in time
   ;; (= (+ (start-time event) (duration event))
   ;;    (start-time next-event))
   (group-events
    :initarg :group-events
    :initform nil)
   ;; does the render-mode need information on tempo/bar-structure?
   (make-notation-data
    :initarg :make-notation-data
    :initform nil)))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric render-events (events render-mode
			   &optional protagonist print))

;; prevent error if render-events is calld with no
;; render-modes. Simply print a warning!
(defmethod render-events (events (render-mode (eql nil))
			  &optional protagonist print)
  (declare (ignore protagonist render-mode print))
  (format
   t
   "~&!!! ERROR: trying to render ~a events with no render-mode."
   (length events)))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* render-mode/make-render-mode
;;; Name
;;; make-render-mode
;;;
;;; File
;;; render-mode.lsp
;;;
;;; Description
;;; Creates a render-mode and adds it to +cc-data+. Render-modes
;;; must always be defined using this function. A render-mode
;;; can later be accessed using get-render-mode. However, when
;;; using it in make-comic, make-event or make-protagonist,
;;; it can be referred to by its name.
;;;
;;; N.B.: make-render-mode does not only create an instance of a
;;; render-mode, it also defines a new method render-events for
;;; each render-mode, which is actually used during render.
;;; This is why creating a render-mode any other way will not
;;; work!
;;;
;;; The different blocks of event code will be executed at
;;; different times of the render-process, as described below.
;;; In each block, a set of variables is accessible. They are:
;;;
;;; Always accessible:
;;; comic, title, subtitle, author, date, output-dir, project-name,
;;; ids (list of ids rendered by mode to current protagonist),
;;; options (each option as var), events (all current events),
;;; render-mode, protagonist, tmp1, tmp2, tmp3, tmp4, tmp5
;;; (tmp variables will be nil but can be used to store any data),
;;; return-file-path (must be set somewhere along the way in order
;;; to return a file and add to to the mix of the protagonist)
;;;
;;; Accessible if make-notation-data is t:
;;; bar-list (cc-bar-objects) , tempo-map (lists with tempo
;;; and time-in-secs), timesig-map (ls with timesig time-in-secs)
;;;
;;; Accessible in before-, event- and after-code only:
;;; event, id (of current event)
;;; N.B.: If group-events=t, the var event will be a list in 
;;; before and after-code, containing possibly subsequent 
;;; events (regarding their position in time)
;;;
;;; Accessible in event-code only:
;;; required and optional slot-values
;;; (e.g.: amplitude holds events amplitude slot-value...)
;;;
;;; Arguments
;;; name: A symbol. It can be used to referre to the render-mode
;;;       in make-comic, make-event or make-protagonist or
;;;       to access the mode-object itself using get-render-mode.
;;; output-type: The type of output-file the render-mode creates
;;;
;;; Optional Arguments (&key)
;;; options: A list of local variables that can be used in
;;;          render-mode-code and changed later using
;;;          setup-render-mode. Syntax is derived from
;;;          let-bindings --> ((var init-val) (var init-val) ...)
;;; required-slots: A list of required event-slots. All required
;;;                 slots will be accessible by their name as
;;;                 variables in render-mode-code.
;;; optional-slots: A list of optional event-slots. All optional
;;;                 slots will be accessible by their name as
;;;                 variables in render-mode-code, but will not
;;;                 necessarily hold a value.
;;; required-packages: A list of lisp-packages the mode requires.
;;; required-software: A list of software-names (symbols) the mode
;;;                    requires. Software can be add using
;;;                    add-software-path.
;;; header-code: Code to execute at the beginning of render, before
;;;              iterating through the events. Will be executed
;;;              once per protagonist.
;;; before-code: Code to execute before event-code. When
;;;              group-events is t, it will be executed once per
;;;              group of events (events that follow each other
;;;              directly without overlapping or rest),
;;;              otherwise once per event.
;;; event-code:  Code to execute once for each event.
;;; after-code:  Code to execute after event-code. When
;;;              group-events is t, it will be executed once per
;;;              group of events, otherwise once per event.
;;; footer-code: Code to execute at the end of render, after
;;;              iterating through the events. Will be executed
;;;              once per protagonist.
;;; group-events: Indicates whether to group events during render,
;;;               if they follow each other directly without
;;;               gap or overlap in time.
;;;
;;; Return Value
;;; A method
;;;
;;; Example
#|
(make-render-mode dummy :sound :required-slots (pitch)
:event-code (print pitch))
|#
;;;
;;; Last Modified
;;; 2020/05/14
;;;
;;; Synopsis
(defmacro make-render-mode (name
			    output-type
			    &key
			      options
			      required-slots
			      optional-slots
			      required-packages
			      required-software
			      header-code
			      before-code
			      event-code
			      after-code
			      footer-code
			      group-events
			      make-notation-data)
;;; ****
  ;; CHECK FOR PROP-NAMES
  (loop for prop in (flat required-slots)
	do
	   (unless
	       (member
		prop
		(loop for slot in (cc-get :event-slots)
		      collect (car slot)))
	     (cc-error 'MAKE-RENDER-MODE
		 "The unit name ~a is no unknown.~%~
              You can add custom properties using (add-event-slot)"
	       prop)))
  ;; CHECK FOR OUTPUT-TYPE
  (unless (member output-type (cc-get :output-types))
    (add-output-type output-type))
  ;; (cc-error 'MAKE-RENDER-MODE
  ;; 	"The output-type ~a is no unknown.~%~
  ;;      You can add custom output-types using (add-output-type)"
  ;;   output-type)
  ;; CHECK FOR EVENT SLOTS
  (let ((event-slots
	  (loop for s in (cc-get :event-slots) collect (car s))))
    (loop for slot in (append required-slots optional-slots)
	  do
	     (unless (member slot event-slots)
	       (add-event-slot slot))))
  ;; group-events may be a form to be evaluated (why? why not?!)
  (setq group-events (eval group-events))
  ;; MAKE THE INSTANCE
  (let* ((instance
	   (make-instance 'render-mode
			  :name name
			  :options options
			  :output-type output-type
			  :required-slots required-slots
			  :optional-slots optional-slots
			  :required-packages required-packages
			  :required-software required-software
			  :header-code header-code
			  :before-code before-code
			  :event-code event-code
			  :after-code after-code
			  :footer-code footer-code
			  :group-events group-events
			  :make-notation-data make-notation-data))
	 (method
	   `(defmethod render-events (events (render-mode (eql ,instance))
				      &optional protagonist print)
	      ;; when rendering without protagonist, a dummy will be
	      ;; created. This way, we can always use the
	      ;; protagonist-var in render-mode-code!
	      (unless protagonist
		(setq protagonist (make-protagonist ,output-type)))
	      (let* ,(append
		      '((comic (cc-get-render-data 'comic))
			(title (cc-get-render-data 'title))
			(subtitle (cc-get-render-data 'subtitle))
			(author (cc-get-render-data 'author))
			(date (cc-get-render-data 'date))
			(output-dir (cc-get-render-data 'output-dir))
			(project-name (name comic))
			(bar-list (cc-get-render-data 'cc-bars))
			(tempo-map (cc-get-render-data 'tempo-map))
			(timesig-map (cc-get-render-data 'timesig-map))
			(ids (mapcar #'id events))
			(tmp1) (tmp2) (tmp3) (tmp4) (tmp5)
			(return-file-path))
		      (loop for opt in options
			    for n from 0
			    collect
			    (list (car opt)
				  `(second
				    (nth ,n (slot-value ,instance 'options))))))
		(declare (ignorable comic title subtitle author date
				    project-name output-dir ids
				    bar-list tempo-map timesig-map
				    tmp1 tmp2 tmp3 tmp4 tmp5))
		(when events
		  ;; run header-code
		  ,header-code
		  ;; group events if specified
		  ,(when group-events
		     '(setq events (cc-group-events events)))
		  (loop for event in events
			do
			   (progn;; progn to ensure evaluable form!
			     ,(when group-events
				;; ensure flat list
				'(setq event (flat events))))
			   ;; run before-code
			   (progn ,before-code);; progn to ensure evaluable form!
			   ;; run event-code, loop in case of grouped events
			   (loop for event in (flat event) do
			     (let ,(loop for slot in
						  (append '(id)
							  required-slots
							  optional-slots)
					 collect
					 (list slot (list slot 'event)))
			       (declare ,(append '(ignorable)
						 '(id) required-slots optional-slots))
			       ;; Information to std-out, when print or verbose is t
			       (if (or print (when-verbose))
				   (format t "~& ... -> ID: ~a, ~
                                  Render-Mode: ~a, Protagonist: ~a~%"
					   id (name render-mode)
					   (if (name protagonist)
					       (name protagonist)
					       "None"))
				   (format t "."))
			       ,event-code))
			   ;; run after-code
			   (progn ,after-code));; progn to ensure evaluable form!
		  ;; run footer-code
		  ,footer-code)
		;; return the return-file-path. It is up to the mode if it is set or
		;; remains nil. It can also be set to t to indicate that something
		;; happend, but not return a path to the protagonist.
		(if (probe-file return-file-path)
		    ;; if a file was created, give it a clear name: protagonist_render-mode
		    (rename-file return-file-path
				 (format nil "~(~a~)_~(~a~)"
					 (name protagonist)
					 (name render-mode)))
		    return-file-path)))))
    ;; add render-mode-name to +cc-data+
    (cc-set :render-modes
	    (acons name instance
		   ;; ensure a render mode with same name is overwritten:
		   (loop for rm in (cc-get :render-modes)
			 unless (eq (car rm) name)
			   collect rm)))
    (format t "~&Render mode ~a was created. You can find it using (get-render-mode '~a)."
	    name name)
    method))

;; print-method for render-modes
(defmethod print-object ((obj render-mode) stream)
  (let ((name (name obj)))
    (format stream "#<RENDER-MODE")
    (when name (format stream ": ~a" name))
    (format stream ">")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* render-mode/get-render-mode
;;; Name
;;; get-render-mode
;;;
;;; File
;;; render-mode.lsp
;;;
;;; Description
;;; Return a render-mode stored in +cc-data+. A render-mode will be
;;; stored in +cc-data+ when created using make-render-mode.
;;;
;;; Arguments
;;; name (usually a symbol). If name is a list, all elements
;;; will be taken as names and a list of render-modes will be
;;; returned.
;;;
;;; Return Value
;;; render-mode or list
;;;
;;; Last Modified
;;; 2020/06/24
;;;
;;; Synopsis
(defun get-render-mode (name)
  "get one or more render-modes by name or list"
;;; ****
  (cond ((listp name)
	 (loop for elem in name collect
				(get-render-mode elem)))
	((typep name 'render-mode)
	 name)
	((symbolp name)
	 (cdr (assoc name (cc-get :render-modes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* render-mode/setup-render-mode
;;; Name
;;; setup-render-mode
;;;
;;; File
;;; render-mode.lsp
;;;
;;; Description
;;; Change the value of an option of a render-mode.
;;; - If no option and value is specified, the function will print
;;; all possible options and current values to standard-out.
;;; - If an option is specified, but no value, the current-value
;;; will be returned.
;;; - If an option and a value is specified, a new value of the
;;; render-mode option will be set.
;;;
;;; N.B.: No value will return the current value, while a value
;;; of nil will set the option to nil.
;;;
;;; Arguments
;;; render-mode: render-mode or symbol (name of the render-mode)
;;;
;;; Optional Arguments
;;; option (symbol): The option to access or set
;;; value: The new value. Will only be set if specified.
;;;
;;; Return Value
;;; boolen or other
;;;
;;; Last Modified
;;;
;;; Synopsis
(defun setup-render-mode (render-mode &optional
					option
					(value nil value-set))
;;; ****
  ;; if render-mode is a symbol, look for the object
  (when (symbolp render-mode)
    (let ((mode (get-render-mode render-mode)))
      (if mode
	  (setq render-mode mode)
	  (cc-error 'SETUP-RENDER-MODE
	      "Render mode ~a could not be found."
	    render-mode))))
  ;; check if type is render-mode 
  (unless (typep render-mode 'render-mode)
    (cc-error 'SETUP-RENDER-MODE
	"~a is not a render-mode."
      render-mode))
  (let ((options (slot-value render-mode 'options))
	(return-val nil))
    (cond (;; if option and value are given, set new value
	   (and option value-set)
	   (setf (slot-value render-mode 'options)
		 (loop for opt in options collect
					  (if (eq option (car opt))
					      (progn
						(setq return-val t)
						(list option value))
					      opt))))
	  ;; if only option, return the current value
	  (option
	   (loop for opt in options do
	     (when (eq option (car opt))
	       (setq return-val (second opt)))))
	  ;; otherwise, print a list of options and
	  ;; current values
	  (t
	   (loop for opt in options do
	     (format t "~&~a: ~a"
		     (first opt) (second opt)))))
    return-val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
