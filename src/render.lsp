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
;; render/render.lsp                                                         2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****ih* cc/render
;;; Name
;;; render
;;;
;;; File
;;; render.lsp
;;;
;;; Description
;;; The render-module, responsible for:
;;; - inheritance of properties
;;; - auto-completing time-structure
;;; - finding render-mode and protagonist for events
;;; - passing events to render-modes
;;; - render events
;;;
;;; Classes
;;; protagonist
;;; render-mode
;;;
;;; Last Modified
;;; 2020/02/07
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; No documentation for most functions in this file, as they are 
;;; meant to be called automatically during render.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-check-dependencies (render-modes)
  (loop for render-mode in (flat render-modes) do
       (when (symbolp render-mode)
	 (setq render-mode (get-render-mode render-mode)))
       (loop for pkg in (slot-value render-mode 'required-packages)
	  do
	    (unless (find-package pkg)
	      (cc-warn
	       'RENDER
	       "The package ~a can not be found.~%~
           Render-mode ~a may not work correctly."
	       pkg (name render-mode))))
       (loop for program in (slot-value render-mode 'required-software)
	  do
	    (unless (assoc program (cc-get :external-software))
	      (cc-warn
	       'RENDER
	       "The path to external program ~a can not be found.~%~
           Render-mode ~a may not work correctly."
	       program (name render-mode)))))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-can-render-with-mode (event render-mode)
  "check if an event has all values a render-mode requires"
  (loop for prop in
       (flat (slot-value render-mode 'required-slots))
     do
       (unless (funcall prop event)
	 (return))
     finally (return t)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-pre-select-render-modes (comic)
  "remove all render-modes from events that have unset required values"
  (doevents (event comic)
    (let ((modes nil))
      (loop for render-mode in (flat (render-modes event))
	 do;; if the mode is not the object but the name,
	 ;;   replace it with the render-mode-object 
	   (unless (typep render-mode 'render-mode)
	     (setq render-mode
		   (let ((mode-obj
			  (get-render-mode render-mode)))
		     (if mode-obj
			 mode-obj
			 (cc-error 'RENDER
			     "The render-mode ~a could not be found.~%~
                              Make sure to always use (make-render-mode) ~
                              to create a render-mode." render-mode)))))
	   (when (cc-can-render-with-mode event render-mode)
	     (push render-mode modes)))
      (render-modes event modes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-group-events (events)
  "grouping events that follow each other directly in time 
   into sublists --> (= st1 (+ st2 dur1))"
  (cond ((eventp events)
	 events)
	((not (listp events))
	 (error "IN GROUP-EVENTS: Argument must be a list.~%~
                 Your argument: ~g" events))
	((not (all? #'eventp events))
	 (error "IN GROUP-EVENTS: All elements in events must ~
            be of type EVENT.~% Your argument: ~g" events))
	(t (labels ((helper (evnts)
		      (if (null (cdr evnts))
			  evnts
			  (let ((prev (if (listp (car evnts))
					  (car (last (car evnts)))
					  (car evnts)))
				(next (cadr evnts)))
			    (if (and (duration prev)
				     (start-time prev)
				     (start-time next))
				(if (u= (u+ (duration prev)
					    (start-time prev))
					(start-time next))
				    (helper (cons
					     (if (listp (car evnts))
						 (append (car evnts)
							 (list next))
						 (list (car evnts)
						       next))
					     (cddr evnts)))
				    (cons (car evnts)
					  (helper (cdr evnts))))
				(cons (car evnts)
				      (helper (cdr evnts))))))))
	     (helper (flat events))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-find-protagonist (event protagonists &optional print)
  "Finds the closest possible protagonist for an event and pushes a clone
   of the event into the protagonists event-list, setting the render-mode
   to the first possible and the location to the location on the objects
   surface."
  (let* ((modes)
	 (modes-filtered)
	 (pt-on-obj)
	 (min-dist)
	 (the-obj))
    ;; we will modify the event a lot from here on
    ;; this is why a clone will be passed to the protagonist,
    ;; not the original from the comic
    (setq event (clone event))
    (loop for obj in (flat protagonists)
       do
	 (setq modes
	       ;; modes are either present in event and obj
	       ;; render-modes-lists...
	       (cond ((and (render-modes obj)
			   (render-modes event))
		      (loop for m in (render-modes event)
			 when (member m (render-modes obj))
			 collect m))
		     ;;... or must match the output-type of obj
		     ((render-modes event)
		      (loop for m in (render-modes event)
			 when (eq (output-type m) (output-type obj))
			 collect m))
		     ;;... or will simply be set to objs mode-ls,
		     ;; filtered by the values present in event
		     (t
		      (loop for m in (render-modes obj)
			 when 
			   (cc-can-render-with-mode event m)
			 collect (get-render-mode m)))))
       ;; when matching modes were found...
	 (when modes
	   ;; ...calculate the distance...
	   (let ((dist
		  (get-distance obj event)))
	     ;; ...find closest possible protagonist
	     (when (or (not min-dist) (< dist min-dist))
	       (setq the-obj obj
		     min-dist dist
		     modes-filtered modes
		     pt-on-obj
		     (get-location-on-surface event obj))))))
    (if the-obj
	;; when a protagonist was found, set...
	(progn
	  ;; ... the render-modes to the first filtered mode
	  (render-modes event (car modes-filtered))
	  ;; ... the location to the location on the surface of obj
	  ;;     relative to its expansion (meaning: location
	  ;;     values will now range from 0 to 1)
	  (location event
		    (loop for pt in pt-on-obj
		       for loc in (location the-obj)
		       for exp in (expansion the-obj) collect
			 (if (zerop exp) 0 (/ (- pt loc) (* 1/2 exp)))))
	  ;; ...the expansion to the expansion in relation to the
	  ;;    protagonists expansion (also zero to one)
	  (expansion event
		     (loop for ex-e in (expansion event)
			for ex-o in (expansion the-obj) collect
			  (if (or (zerop ex-e) (zerop ex-o))
			      0
			      (clip
			       (/ ex-e ex-o) 0 1))))
	  ;; ... and push the event into the list of events of obj
	  (events the-obj (cons event (events the-obj)))
	  t)
	;; warn if no protagonist was found
	(when print
	  (format
	   t "~&WARNING: No protagonist found for event with id ~d"
	   (id event))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-get-render-data (key)
  (second (assoc key +cc-render-data+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-set-render-data (key val)
  (let ((ls (list key val)))
    (push ls +cc-render-data+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ensure-protagonists (objs)
  "Makes sure objs is a flat list containing only protagonists"
  (loop for ren-ob in (flat objs)
     collect
       (progn
	 (when (symbolp ren-ob)
	   (setq ren-ob
		 (get-protagonist ren-ob)))
	 (if (typep ren-ob 'protagonist)
	     ren-ob
	     (cc-error 'RENDER
		 "~a is not a protagonist."
	       ren-ob)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* comic/render
;;; Name
;;; render
;;;
;;; File
;;; render.lsp
;;;
;;; Description
;;; Render a comic (or an event) to one or more protagonists.
;;;
;;; Arguments
;;; - comic: The object to render. Can also be an event or a list
;;;   of events
;;; - protagonists: One or more protagonists to render to
;;;
;;; Optional Arguments
;;; - output-dir: Overwrite the default output directory
;;; - print: Print additional information on the render-process
;;; - auto-complete-time: Auto-complete start-times and durations
;;;   The algorithm should be able to always auto-complete
;;;   all values if one or more non-zero time-values are
;;;   present.
;;; - protagonists-to-ignore: A list of protagonists to ignore
;;;   during render. The protagonist will be considered when
;;;   selecting events, but events will not be rendered.
;;; - mix: Indicates whether a mix method should be called if
;;;   it is defined for a given output-type. (default: t)
;;; - tidy-up: Indicates whether the temporary directory should
;;;   be tidied up after render. All files created by
;;;   in tmp-dir during render will be deleted. Other
;;;   files in tmp-dir will not be deleted!
;;;   (delete-temporary-files does this job.)
;;;
;;; Return Value
;;; T
;;;
;;; Example
#|
(render #<COMIC ...> (make-protagonist :sound) :print t)
|#
;;;
;;; Last Modified
;;; 2020/05/14
;;;
;;; Synopsis
(defgeneric render (comic protagonists
		    &rest args
		    &key
		      print
		      auto-complete-time
		      call-space-superhero
		      output-dir
		      mix
		      tidy-up
		      &allow-other-keys))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; :before-methods prepare the render-process                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render :before ((obj comic) protagonists
			   &rest args &key
					print
			   &allow-other-keys)
  (declare (ignore args))
  (when print (cc-set :verbose t))
  (cc-set-render-data 'comic obj)
  (cc-set-render-data 'title (title obj))
  (cc-set-render-data 'subtitle (subtitle obj))
  (cc-set-render-data 'author (author obj))
  (cc-set-render-data 'date (date obj))
  (cc-set-render-data 'project-name (name obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render :before ((obj event) protagonists
			   &rest args
			   &key
			     print
			     (auto-complete-time t)
			     (call-space-superhero t)
			   &allow-other-keys)
  (declare (ignore args))
  ;;set project name if not set by comic-name before
  (unless (cc-get-render-data 'project-name)
    (cc-set-render-data 'project-name (cc-get :current-project-name)))
  ;;find and set protagonists
  (cc-set-render-data 
   protagonists
   (loop for ren-ob in (flat protagonists)
	 collect
	 (if (symbolp ren-ob)
	     (get-protagonist ren-ob)
	     ren-ob)))
  ;; if a single event should be rendered, we insert a dummy as subevent
  ;; that will inherit all slots from the parent. Simple solution.
  (unless (events obj)
    (events obj (make-event)))
  (when call-space-superhero
    (call-superhero space-superhero obj))
  ;; make sure the comic-object is prepared
  (prepare obj print auto-complete-time t)
  ;; check if the modes specified in subevents
  ;; have unavailable dependencies
  (doevents (e (events obj))
    (cc-check-dependencies (render-modes e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render :before ((obj placed-object) protagonists
			   &rest args
			   &key
			     output-dir
			     &allow-other-keys)
  (declare (ignore args))
  ;;set out-dir to default or to arg if specified
  (if output-dir
      (cc-set-render-data 'output-dir output-dir)
      (cc-set-render-data 'output-dir (cc-get :output-dir)))
  ;; check if the modes have any
  ;; unavailable dependencies
  (cc-check-dependencies (render-modes obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The actual render methods will pass data to a specific method;;;
;;; "render-events", generated by macro "make-render-mode"       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For lists: simply render each item
(defmethod render ((obj list) protagonists
		   &rest args &key &allow-other-keys)
  (loop for elem in obj do
       (apply #'render elem protagonists args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render ((obj comic)
		   protagonists
		   &rest args
		   &key
		     &allow-other-keys)
  (declare (ignore args))
  ;; Append comic-name as folder-name to output-dir
  (when (name obj)
    (cc-set-render-data 'output-dir
			(cc-concat-paths
			 (cc-get-render-data 'output-dir)
			 (name obj))))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render ((obj event)
		   protagonists
		   &rest args
		   &key
		     protagonists-to-ignore
		     (mix t)
		     print
		   &allow-other-keys)
  (declare (ignore args))
  ;;find protagonists by names and put them in a flat list:
  (setq protagonists (ensure-protagonists protagonists))
  (setq protagonists-to-ignore
	(ensure-protagonists protagonists-to-ignore))
  (let* ((output-dir (cc-get-render-data 'output-dir))
	 (success nil))
    (format t "~&Starting to render to ~a" output-dir)
    ;; pre-select render modes by required-slots
    (when (or print (when-verbose))
      (format t "~&Pre-selecting render-modes..."))
    (cc-pre-select-render-modes obj)
    ;; the render-process:
    (when (or print (when-verbose))
      (format t "~&Selecting protagonists for events..."))
    (doevents (e obj)
      (cc-find-protagonist e protagonists print))
    (labels (;; The helper function renders all events to one protagonist
	     ;; It looks for the mode of the first object, renders all events
	     ;; with that mode and repeats the process as long as any events are
	     ;; left.
	     (helper (events protagonist)
	       (let (;; set current-mode to render-mode of first event
		     (current-mode (render-modes (car events)))
		     (current-events)
		     (remaining-events))
		 ;; select all events with current mode
		 (loop for e in events collect
		      (if (eq current-mode (render-modes e))
			  (push e current-events)
			  (push e remaining-events)))
		 ;; render events with current-mode to
		 ;; current-object
		 (when (and current-mode current-events)
		   ;; ensure the events are sorted by start-time
		   (sort-by-start-time current-events)
		   ;; (when (or print (when-verbose))
		   ;;   (format
		   ;;    t "~&Rendering event(s) with ID(s) ~{~a~^, ~}~%~
                   ;;          with render-mode ~a ~%to protagonist ~a..."
		   ;;    (mapcar #'id current-events) (name current-mode)
		   ;;    (name protagonist)))
		   (let ((return-file
			  ;; Call to render-mode-code happens here:
			  (render-events current-events
					 current-mode
					 protagonist
					 print)))
		     (when (type-or return-file '(string pathname))
		       (push return-file
			     (slot-value protagonist 'output-files)))
		     (unless return-file
		       (format
			t "~&Render-mode ~a returned nil after call with~%~
                            protagonist ~a. Maybe the render process failed."
			current-mode protagonist))
		     (when remaining-events
		       (helper remaining-events protagonist)))))))
      (loop for ren-obj in protagonists do
	   ;; do not render protagonist if on ignore-list
	   (unless (member ren-obj protagonists-to-ignore)
	     (format t "~&Rendering ~d events to protagonist ~a"
		     (length (events ren-obj)) (name ren-obj))
	     (helper (events ren-obj) ren-obj)
	     (when (output-files ren-obj)
	       (setq success t))
	     (if mix
		 (mix (output-type ren-obj)
		      (absolute-path
		       (format nil "~a.~a"
			       (name ren-obj)
			       (get-output-type-suffix
				(output-type ren-obj))))
		      (output-files ren-obj)
		      :print print
		      ;; :sample-rate (cc-get :sample-rate)
		      )
		 (loop for file in (output-files ren-obj)
		    do
		      (cp-to-project-dir file))))
	   ;; reset the object
	   (setf (output-files ren-obj) nil)
	   (events ren-obj nil)))
    (format t "~&RENDER PROCESS FINISHED.~%~
               Files rendered to ~a~%----------~%" output-dir)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod render :after ((obj placed-object) protagonists
			  &rest args
			  &key
			    (tidy-up t)
			    print
			    &allow-other-keys)
  (declare (ignore args))
  (setq +cc-render-data+ nil)
  (when print (cc-set :verbose nil))
  (when tidy-up
  ;; temporary files created using make-tmp-file or output-tmp-file
  ;; will be stored in in +cc-data+ by key :temporary-files. The
  ;; tmp-dir will be cleaned using delete-temporary-files. This is
  ;; especially important to ensure that new files can be written
  ;; in next render-process! (Some external software may not
  ;; overwrite existing files, etc.)
    (delete-temporary-files)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF render.lsp
