(in-package :comic)


(clim:define-application-frame comic-editor ()
  ;; The slots
  ((the-comic :accessor the-comic
	      :initform (make-comic 'untitled)
	      :initarg :the-comic)
   (selected-element :accessor selected-element :initform nil)
   (x-slot :accessor x-slot :initform 'location)
   (y-slot :accessor y-slot :initform 'location)
   (x-get :accessor x-get
	  :initform (lambda (e) (first (flat (location e)))))
   (y-get :accessor y-get
	  :initform (lambda (e) (second (flat (location e)))))
   (x-set :accessor x-set
	  :initform
	  (lambda (e val)
	    (when (null (location e)) (location e '(0)))
	    (location e (set-nth (flat (location e)) 0 val))))
   (y-set :accessor y-set
	  :initform
	  (lambda (e val)
	    (let ((loc (location e)))
	      (when (< (length loc) 2) (setq loc (set-length 2 loc)))
	      (location e (set-nth loc 1 val)))))
   (x-min :accessor x-min :initform nil)
   (x-max :accessor x-max :initform nil)
   (y-min :accessor y-min :initform nil)
   (y-max :accessor y-max :initform nil)
   (app-height :accessor app-height :initform 0)
   (app-width :accessor app-width :initform 0)
   (x-zoom :accessor x-zoom :initform 100)
   (y-zoom :accessor y-zoom :initform 100)
   (x-unit :accessor x-unit :initform nil)
   (y-unit :accessor y-unit :initform nil)
   (filter-value :accessor filter-value :initform 0)
   (apply-filter :accessor apply-filter :initform nil)
   )
   ;;
  (:pointer-documentation t)
  (:menu-bar menubar-command-table)
  (:icon (clim:make-pattern-from-bitmap-file
	  (src-dir "comic-icon.png")
  	  :format :png))
  (:panes
   (app :application
	:height 850
	:width 900
	:scroll-bars t
	:display-function #'display-app
	:background (clim:make-rgb-color 0.95 0.95 0.95)
	:text-style (clim:make-text-style :fix :roman 12))
   (set-view 
    (clim:horizontally ()
      ;; macht es sinn, so "durch die Zeit" zu gehen? Woher kommen die
      ;; Werte dafür (vor flatte-event-list)
      (clim:make-pane :slider
		      :value-changed-callback #'callback-filter-slider
		      :min-value 0
		      :max-value 1
		      :value 0
		      :show-value-p nil
		      :orientation :horizontal)
      (clim:make-pane :slider
		      :label "Zoom"
		      :value-changed-callback #'callback-zoom-slider
		      :min-value 25
		      :max-value 1000
		      :value 100
		      :show-value-p t
		      :orientation :horizontal)))
   (tree :application
	 :height 400
	 :width 200
	 :display-function #'display-tree
	 :scroll-bars nil
	 :text-style (clim:make-text-style :fix :roman 12))
   (sel :application
	:height 300
	:width 200
	:scroll-bars nil
	:text-style (clim:make-text-style :fix :roman 12)
	:display-function 'display-sel)
   (int :interactor
   	:text-style (clim:make-text-style :fix :roman 12)
	:scroll-bars nil
   	:height 200
   	:width 200)
   )
  (:layouts
   (default
    (clim:horizontally ()
      (clim:scrolling (:width 200) (clim:vertically () tree sel ;; int
						    ))
      (clim:vertically () app set-view)))))

(defgeneric draw (obj x1 x2 y1 y2 color))

(defmethod draw ((obj event) x1 x2 y1 y2 color)
  (let ((stream (clim:get-frame-pane clim:*application-frame* 'app)))
    (clim:with-output-as-presentation
	(stream obj 'event)
      (clim:draw-rectangle*
       stream x1 y1 x2 y2
       :ink (apply #'clim:make-rgb-color color) 
       :filled t))))

(defun get-width-height (pane)
  (let* ((reg (slot-value (clim:sheet-region pane)
			  'clim-internals::coordinates)))
    (values (- (aref reg 2) (aref reg 0))
	    (- (aref reg 3) (aref reg 1)))))


(defun val->coordinate (val v-min v-max pix-min pix-max zoom)
  ;; 40px margin on each side
  (let* ((mar (if (> pix-min pix-max) -40 40))
	 (coor (* (shift-value val v-min v-max
			       (+ pix-min mar) (- pix-max mar))
		  (/ zoom 100))))
    ;; rather be safe here, as we use the values to draw on the
    ;; screen: 
    ;; (clip coor (min pix-min pix-max) (max pix-min pix-max))))
    coor))

  (defun coordinate->val (coor v-min v-max pix-min pix-max zoom)
  ;; 40px margin on each side
  (let ((mar (if (> pix-min pix-max) -40 40)))
    (shift-value (/ coor (/ zoom 100)) ; inverse zoom factor
		 (+ pix-min mar) (- pix-max mar)
		 v-min v-max)))


(defun update-display-app-data ()
  (let* ((pane (clim:get-frame-pane clim:*application-frame* 'app))
	 (x-slot (x-slot clim:*application-frame*))
	 (y-slot (y-slot clim:*application-frame*))
	 (x-get (x-get clim:*application-frame*))
	 (y-get (y-get clim:*application-frame*))
	 (x-unit (x-unit clim:*application-frame*))
	 (y-unit (y-unit clim:*application-frame*))
	 (the-comic (the-comic clim:*application-frame*))
	 (pane-width) (pane-height)
	 (x-min) (x-max) (y-min) (y-max)
	 (placed-objects))
    ;; get height and width of pane
    (multiple-value-bind (w h)
	(get-width-height pane)
      (setq pane-width w
	    pane-height h))
    ;; get all placed objects involved:
    (doevents (event the-comic) (push event placed-objects))
    ;; get min and max values per axis:
    (loop for plob in placed-objects
	  do
	     (let* ((slots (get-slots plob))
		    (slots? (and (member x-slot slots)
				 (member y-slot slots))))
	       (when slots?
		 (let ((x-val (value (funcall x-get plob) x-unit))
		       (y-val (value (funcall y-get plob) y-unit)))
		   (when x-val
		     (when (or (null x-min) (> x-min x-val))
		       (setq x-min x-val))
		     (when (or (null x-max) (< x-max x-val))
		       (setq x-max x-val)))
		   (when y-val
		     (when (or (null y-min) (> y-min y-val))
		       (setq y-min y-val))
		     (when (or (null y-max) (< y-max y-val))
		       (setq y-max y-val)))))))
    ;; make sure we have all min/max ranges:
    (unless x-min (setq x-min 0))
    (unless y-min (setq y-min 0))
    (when (or (not x-max) (= x-max x-min))
      (setq x-max (1+ x-min)))
    (when (or (not y-max) (= y-max y-min))
      (setq y-max (1+ y-min)))
    (setq x-min (min x-min 0)
	  y-min (min y-min 0)
	  x-max (max x-max 1)
	  y-max (max y-max 1))
    ;; set slot-values in application-frame:
    (setf (x-min clim:*application-frame*) x-min
	  (x-max clim:*application-frame*) x-max
	  (y-min clim:*application-frame*) y-min
	  (y-max clim:*application-frame*) y-max
	  (app-height clim:*application-frame*) pane-height
	  (app-width clim:*application-frame*) pane-width)))


    
;; (defun convert-coordinate () ...)
;; MORE ABSTRACTIONS!
(defun display-app (frame pane)
  (declare (ignore frame))
  ;; make sure all display-values are valid:
  (update-display-app-data)
  (let* ((x-slot (x-slot clim:*application-frame*))
	 (y-slot (y-slot clim:*application-frame*))
	 (x-get (x-get clim:*application-frame*))
	 (y-get (y-get clim:*application-frame*))
	 (x-unit (x-unit clim:*application-frame*))
	 (y-unit (y-unit clim:*application-frame*))
	 (x-zoom (x-zoom clim:*application-frame*))
	 (y-zoom (y-zoom clim:*application-frame*))
	 (sel (selected-element clim:*application-frame*))
	 (sel-id (when (eventp sel) (id sel)))
	 (sel-child-ids nil)
	 (the-comic (the-comic clim:*application-frame*))
	 (pane-width (app-width clim:*application-frame*))
	 (pane-height (app-height clim:*application-frame*))
	 (x-min (x-min clim:*application-frame*))
	 (x-max (x-max clim:*application-frame*))
	 (y-min (y-min clim:*application-frame*))
	 (y-max (y-max clim:*application-frame*)))
    ;; draw coordinate grid
    (clim:with-room-for-graphics (pane :first-quadrant nil)
      (let* ((x0
	       (when (<= y-min 0 y-max)
		 (val->coordinate 0 y-min y-max pane-height 0 y-zoom)))
	     (y0
	       (when (<= x-min 0 x-max)
		 (val->coordinate 0 x-min x-max 0 pane-width x-zoom))))
	;; x-axis
	(when x0
	  (clim:draw-arrow* pane 0 x0 pane-width x0
			    :line-thickness 2)
	  ;; (clim:draw-text* pane (format nil "~a" x-unit)
	  ;; 		   (- pane-width 200) x0
	  ;; 		   :text-style (clim:make-text-style
	  ;; 				:sans-serif :bold 14))
	  )
	;; y-axis
	(when y0
	  (clim:draw-arrow* pane y0 pane-height y0 0
			    :line-thickness 2)
	  ;; (clim:draw-text* pane (format nil "~a" y-unit)
	  ;; 		   (- y0 80) 40
	  ;; 		   :text-style (clim:make-text-style
	  ;; 				:sans-serif :bold 14))
	  )
        (let ((x-grid-steps (/ (- x-max x-min) 10))
	      (y-grid-steps (/ (- y-max y-min) 10)))
	  (when x0
	    (loop for x from x-min to x-max by x-grid-steps
		  for x-pix = (val->coordinate x x-min x-max 0
					       pane-width x-zoom)
		  do
		     ;; (clim:draw-line* pane
		     (clim:draw-text* pane (format nil "| ~a"
						   (round-to-n x 3))
				      x-pix x0)))
	  (when y0
	    (loop for y from y-min to y-max by y-grid-steps
		  for y-pix = (val->coordinate y y-min y-max
					       pane-height 0 y-zoom)
		  do
		     ;; (clim:draw-line* pane
		     (clim:draw-text* pane (format nil "– ~a"
						   (round-to-n y 3))
				      y0 y-pix))))))
    ;; (draw-line* stream 0 0 200 0
    ;;                          :line-thickness i
    ;;                          :line-dashes (list (* i 2) (round i 2)))
    ;; collect all children if selection is an event:
    (when sel-id
      (doevents (c (events sel))
	(push (id c) sel-child-ids)))
    ;; now draw events:
    (doevents (event the-comic)
      (let* ((slots (get-slots event))
	     (draw? (and (member x-slot slots)
			 (member y-slot slots))))
	(when draw?
	  (let* ((x-val (value (funcall x-get event) x-unit))
		 (y-val (value (funcall y-get event) y-unit)))
	    (when (and x-val y-val)
	      (let* ((x1 (val->coordinate
			  x-val x-min x-max 0 pane-width x-zoom))
		     (y1 (val->coordinate
			  y-val y-min y-max pane-height 0 y-zoom))
		     (x2 (+ x1 8)) (y2 (+ y1 8))
		     (color
		       (cond (;; selected event is red:
			      (equal sel event)
			      '(1 0.2 0.2))
			     ;; selections children are black:
			     ((member (id event) sel-child-ids)
			      '(1 0.75 0))
			     ;; other events are gray:
			     (t '(.75 .75 .75)))))
		(draw event x1 x2 y1 y2 color)))))))))



(defun display-set-view (frame pane)
  (declare (ignore frame pane)) t)

(defun callback-filter-slider (slider value)
  (declare (ignore slider))
  (setf (filter-value clim:*application-frame*) value))

(defun callback-zoom-slider (slider value)
  (declare (ignore slider))
  (setf (x-zoom clim:*application-frame*) value)
  (setf (y-zoom clim:*application-frame*) value))

;;; Display the structure-tree of the comic:
(defun display-tree (frame pane)
  (declare (ignore frame))
  (let ((selected-element (selected-element clim:*application-frame*))
	(the-comic (the-comic clim:*application-frame*)))
    (labels ((print-events (events tab)
	       (unless (listp events)
		 (setq events (list events)))
	       (loop for event in events
		     do
			(clim:with-output-as-presentation
			    (pane event 'event)
			  (if (> tab 0)
			      (format pane (format nil "~~&~~~dT" (* 3 tab)))
			      (format pane "~&"))
			  (if (comicp event)
			      (format pane "<comic ~a>" (name event))
			      (format pane "<event ~a>" (id event)))
			  (when (and
				 (eventp selected-element)
				 (id event)
				 (id selected-element)
				 (= (id event)
				    (id selected-element)))
			    (format pane " (selected)")))
			(when (events event)
			  (print-events
			   (events event) (1+ tab))))))
      (print-events the-comic 0))))


;;; Display the info on selected element in Selection-Pane:
(defun display-sel (frame pane)
  (declare (ignore frame))
  (let ((sel (selected-element clim:*application-frame*)))
    (info sel :stream pane :print-event-structure? nil)))

(defun accept-and-eval (prompt)
  (eval (read-from-string (clim:accept 'string :prompt prompt))))

(defun value-dialog (title &rest names-types-defaults)
  (let ((stream (clim:get-frame-pane clim:*application-frame* 'app)))
    (clim:window-clear stream)
    (let ((vals
	    (clim:accepting-values (stream :own-window nil
					   :resynchronize-every-pass nil)
	      (clim:with-room-for-graphics (stream :height 50 :first-quadrant nil)
		(clim:draw-text* stream (format nil "~:(~a~)" title)
				 0 0 :text-style (clim:make-text-style
						  :sans-serif :bold 24)))
	      (loop for (name type default) in names-types-defaults
		    collect
		    (progn
		      (fresh-line stream)
		      (if default
			  (clim:accept
			   type
			   :prompt (format nil "~:(~a~)" name)
			   :default default
			   :stream stream)
			  (clim:accept
			   type
			   :prompt (format nil "~:(~a~)" name)
			   :stream stream)))))))
      (if (= 1 (length vals))
	  (car vals) ; if only one query, return the atom directly
	  vals)))) ; else, return a list of entered values

;;;
;;; Comic-Editor-Commands
;;;
;; DELETE AN EVENT
(define-comic-editor-command (com-delete :name t) ((event event))
  (delete-event (the-comic clim:*application-frame*) (id event)))

(clim:define-presentation-to-command-translator
    delete
    (event com-delete comic-editor :gesture nil)
    (object)
  (list object))

;; ADD A NEW EVENT
(define-comic-editor-command (com-add-event :name t) ((event event))
  (events event (append (events event) (list (make-event))))
  (cc-assign-ids-to-events (the-comic clim:*application-frame*)))

(clim:define-presentation-to-command-translator
    add-event
    (event com-add-event comic-editor :gesture nil)
    (object)
  (list object))

;;; menu-choose --> auch interessant?
(define-comic-editor-command (com-edit :name t) ((obj named-object))
  (let ((stream (clim:get-frame-pane clim:*application-frame* 'app))
	(ignore-slots '(events id)))
    (clim:window-clear stream)
    (clim:accepting-values (stream :own-window nil
				   :resynchronize-every-pass t)
      (clim:with-room-for-graphics (stream :height 50 :first-quadrant nil)
	(clim:draw-text* stream (format nil "Edit ~:(~a~)" (type-of obj))
			 0 0 :text-style (clim:make-text-style
					  :sans-serif :bold 24)))
      (loop for slot-name in (reverse (get-slots obj))
	    unless (member slot-name ignore-slots)
	      do
		 (let* ((val (slot-value obj slot-name))
			(new-val nil))
		   (fresh-line stream)
		   (setq new-val
			 (clim:accept
			  'clim:form
			  :prompt
			  (if val
			      (format nil "~:(~a~) (~:(~a~))" slot-name val)
			      (format nil "~:(~a~)" slot-name))
			  :stream stream))
		   (when new-val
		     (setf (slot-value obj slot-name) (eval new-val))))))))

(clim:define-presentation-to-command-translator
    edit
    (named-object com-edit comic-editor :gesture nil)
    (object)
  (list object))


;; SELECT AN OBJECT
(define-comic-editor-command (com-select :name t) ((obj named-object))
  (setf (selected-element clim:*application-frame*) obj))

(clim:define-presentation-to-command-translator
    select
    (event com-select comic-editor :gesture :select)
    (object)
  (list object))

;; UPDATE ALL DATA (after manual changes in lisp)
(define-comic-editor-command (com-update :name t) ()
  t)

(define-comic-editor-command (com-eval :name t) ((expr 'string))
  (eval (read-from-string expr)))



;;;
;;; Menu-bar only commands:
;;;
(define-comic-editor-command (com-new :name t) ()
  (setf (the-comic clim:*application-frame*)
	(make-comic
	 (value-dialog "New Comic"
		       '(name symbol)))))

(define-comic-editor-command (com-save :name t :menu t) ()
  (save (the-comic clim:*application-frame*)
	(value-dialog "Save Comic to File"
		      (list "Filepath" 'string
			    (namestring (user-homedir-pathname))))))

(define-comic-editor-command (com-load :name t) ()
  (let* ((obj (load-and-return
	       (value-dialog "Load Comic from File"
			     (list "Filepath" 'string
				   (namestring (user-homedir-pathname)))))))
    (if (comicp obj)
	(setf (the-comic clim:*application-frame*) obj)
	(format *standard-output*
		"The selected file is not a saved comic."))))

(define-comic-editor-command (com-load-from-lisp :name t) ()
  (let* ((obj (value-dialog "Load Comic from Lisp"
			    '("Associated Symbol" clim:form))))
    (when (symbolp obj) (setq obj (eval obj)))
    (if (comicp obj)
	(setf (the-comic clim:*application-frame*) obj)
	(format *standard-output*
		"The associated object is not a comic."))))


;;; TODO: MAKE PREFERENCE MENU
;; (define-comic-editor-command (com-preferences :name t) ()
;;   (let ((prefs
;; 	  (value-dialog "Preferences..."
;; 			(list "Render-mode filenames"
;; 			      '(sequence string)
;; 			      (cc-get :preferences
;; 				      :render-mode-files))
;; 			;; `("Software Paths"
;; 			;;  clim:sequence
;; 			;;  ,(cc-get :preferences :software-paths))
;; 			)))
;;     prefs))


(define-comic-editor-command (com-quit :name t) ()
  (clim:frame-exit clim:*application-frame*))


;; MAKE THE MENU BAR:
(clim:make-command-table 'menubar-command-table
			 :errorp nil
			 :menu '(("Comic" :menu comic-command-table)
				 ("Event" :menu event-command-table)))

(clim:make-command-table
 'comic-command-table
 :errorp nil
 :menu '(("New" :command com-new)
	 ("Save to file" :command com-save)
	 ("Load from file" :command com-load)
	 ("Load from lisp" :command  com-load-from-lisp)
	 ("Preferences..." :command com-preferences)
	 ("Quit" :command com-quit)))

(clim:make-command-table 'event-command-table
                    :errorp nil
                    :menu '())


(defun cc-start-gui-editor (&optional comic)
  (let ((frame (clim:make-application-frame
		'comic-editor
		:pretty-name "Comic Editor"
		:the-comic
		(if comic comic (make-comic 'untitled)))))
    (values frame
	    (clim-sys:make-process
	     (lambda ()
	       (in-package :comic)
	       (clim:run-frame-top-level frame))))))
;;; ADD THE EDITOR TO FEATURES-LIST
(push :comic-gui-editor *features*)



(defun print-on-app (control-string &rest format-arguments)
  (if (stringp control-string)
      (apply #'format (clim:get-frame-pane clim:*application-frame* 'app)
	     control-string format-arguments)
      (format (clim:get-frame-pane clim:*application-frame* 'app)
	      "~a" control-string)))
  


(defun get-pointer-position (pane)
  (multiple-value-bind (x y) (clim:stream-pointer-position pane)
    (list x y)))


(define-comic-editor-command (com-add-event-here)
    ((x number) (y number))
  (let* ((x-set (x-set clim:*application-frame*))
	 (y-set (y-set clim:*application-frame*))
	 (x-unit (x-unit clim:*application-frame*))
	 (y-unit (y-unit clim:*application-frame*))
	 (x-zoom (x-zoom clim:*application-frame*))
	 (y-zoom (y-zoom clim:*application-frame*))
	 (pane-width (app-width clim:*application-frame*))
	 (pane-height (app-height clim:*application-frame*))
	 (x-min (x-min clim:*application-frame*))
	 (x-max (x-max clim:*application-frame*))
	 (y-min (y-min clim:*application-frame*))
	 (y-max (y-max clim:*application-frame*))
	 (sel (selected-element clim:*application-frame*)))
    (when (eventp sel)
      (let* ((event (make-event))
	     (x-val
	       (round-to-5
		(coordinate->val x x-min x-max 0 pane-width x-zoom)))
	     (y-val
	       (round-to-5
		(coordinate->val y y-min y-max pane-height 0 y-zoom))))
	(funcall x-set event (convert x-val x-unit))
	(funcall y-set event (convert y-val y-unit))
	(add-events sel event)
	(cc-assign-ids-to-events (the-comic clim:*application-frame*))))))


(clim:define-presentation-to-command-translator
    translator-add-event-here
    (clim:blank-area com-add-event-here comic-editor
		     :documentation "Add an event here"
		     :tester
		     ((object)
		      (let ((frame clim:*application-frame*))
			(eq (clim:pointer-sheet (clim:port-pointer
						 (clim:port frame)))
			    (clim:get-frame-pane frame 'app)))))
    (object)
  (get-pointer-position
      (clim:get-frame-pane clim:*application-frame* 'app)))

;; (define-dragndrop-command (com-move-object)
;;     ((original circle))
;;   (let ((pane (clim:get-frame-pane *application-frame* 'app)))
;;     (multiple-value-bind (x y)
;; 	(clim:dragging-output (pane :finish-on-release t)
;; 	  (clim:draw-circle pane (get-pointer-position pane)
;; 			    (radius original)
;; 			    :filled nil))
;;       (com-add-circle (make-point x y) (radius original)))))

;; (define-presentation-to-command-translator translator-move-circle
;;     (circle com-move-circle dragndrop)
;;     (object)
;;   (list object))
