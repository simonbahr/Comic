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
;; cc/all.lsp                                                                2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* cc/main
;;; Name
;;; cc
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; The main and entry module, responsible for:
;;; - Definition of the comic-package
;;; - Definition of globals
;;; - Configuration, error, warn and test functions
;;; - Loading modules
;;;
;;; Classes
;;; named-object
;;; timed-object
;;; placed-object
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINE PACKAGE                                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :comic
  (:use :common-lisp)
  (:nicknames :cc))


;;;ENTER THE NEW PACKAGE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)                                                            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-exit () (in-package :cl-user))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GLOBAL DATA                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* main/+cc-data+
;;; Name
;;; +cc-data+
;;;
;;; File
;;; all.lisp
;;;
;;; Description
;;; +cc-data+ holds an association list with all global information
;;; needed in comic. Data can be accessed using cc-get and set
;;; using cc-set. However, this should usually not be necassary to
;;; do by hand.
;;;
;;; Default keys:
;;; :event-slots -> all slots an event-object has. Can be extended
;;;                 using (add-event-slot)
;;; :output-types -> different types of output (:sound, :video, etc.)
;;; :render-modes -> a list of all available render-modes. modes are
;;;                  automatically added when created using
;;;                  make-render-mode.
;;; :protagonists -> a list of all available protagonists. objects
;;;                    are automatically added when created using
;;;                    make-protagonist.
;;; :superheros -> a list of all available supereros, objects are
;;;                automatically addrd when created using
;;;                make-superhero
;;; :current-project-name -> name of current project. can be set
;;;                          manually and will be set to comic-name
;;;                          during render.
;;; :external-program-call -> function for calling external programs
;;;                           (default: #'sb-ext:run-program)
;;; :current-mix (used internally only)
;;; :external-software -> list of external software names and paths.
;;;                       software can be added using add-software-path
;;; :output-dir -> output directory, default: "/tmp/"
;;; :tmp-dir -> directory for storing temporary files,
;;;             default: "/tmp/"
;;; :src-dir -> source-directory, automatically set when loading
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; Synopsis
(defparameter +cc-data+
  ;; available output-types and their default file-formats:
  `((:preferences (:render-mode-files nil)
		  (:software-paths nil)
		  (:output-dir . "/tmp/")
		  (:tmp-dir . "/tmp/")
		  (:external-program-call . sb-ext:run-program))
    (:output-types . (:sound)) ; (mix) depends on :clm
    ;; all defined render-modes will be collected here:
    (:render-modes . nil)
    ;; all defined protagonists will be collected here:
    (:protagonists . nil)
    ;; all defined superheros will be collected here:
    (:superheros . nil)
    ;; all available event-slots
    (:event-slots . ((name symbol)
		     (id number)
		     (start-time numeric-unit (number secs))
		     (duration numeric-unit (number secs))
		     (render-modes list render-mode symbol)
		     (pitch numeric-unit (number hz))
		     (amplitude numeric-unit
				(number amp)
				dynamic-symbol
				(symbol dynamic-symbol))
		     (location number list)
		     (expansion number list)
		     (text string unit)
		     (soundfile soundfile)
		     (videofile videofile)
		     (events event list)))
    ;; name of current comic during render:
    (:current-project-name . nil)
    ;; implementation-based call for running external programs,
    ;; default for sbcl
    (:external-program-call . #'sb-ext:run-program)
    ;; list of internal names and
    ;; filespecs for external program calls:
    (:external-software . ((open . "/usr/bin/xdg-open")))
    ;; the default output directory:
    (:output-dir . "/tmp/")
    ;; the default output directory for temporary files:
    (:tmp-dir . "/tmp/")
    ;; A place, where temporary files are collected.
    ;; Render will delete all current tmp-files
    (:temporary-files . nil)
    ;; The source-directory:
    (:src-dir . ,(namestring
		  (truename 
		   (directory-namestring (or *load-truename* "./")))))
    ;; shared setting for various processes whether to print detailed
    ;; or only basic information to std-out.
    (:verbose . nil)))
;;; ****

(defparameter +cc-render-data+ nil)
;; will only be used in render processed and emptied after each render

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/cc-get
;;; Name
;;; cc-get
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Access global data stored in +cc-data+. For a list of possible
;;; keys see documentation of +cc-data+.
;;;
;;; Arguments
;;; key: The key to look for in +cc-data+
;;;
;;; Return Value
;;; Any data
;;;
;;; Example
#|
(cc-get :src-dir) --> "/home/simon/comic/"
|#
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-get (key &rest keys)
  (labels ((helper (assoc-ls keys)
	     (let* ((key (car keys))
		    (cell (assoc key assoc-ls))
		    (warning "~&WARNING: The key ~a could not be found ~
                              in +cc-data+."))
	       (cond ((null cell)
		      (format t warning key))
		     ((null (cdr keys))
		      (cdr cell))
		     ((listp (second cell))
		      (helper (cdr cell) (cdr keys)))
		     ;; display the next key if there are
		     ;; still keys but the cdr of cell is
		     ;; no list to assoc in
		     ((cdr keys)
		      (format t warning (second keys)))
		     (t
		      (format t warning key))))))
    (helper +cc-data+ (cons key keys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MUST BE DEFINED BEFORE LOADING CLASSES.LSP
;; (cc-get-event-slot-types 'pitch) --> (NUMERIC-UNIT NUMBER)
(defun cc-get-event-slot-types (slot-name)
  "Returns a list of possible types for an event-slot"
  (loop for type in (cc-get :event-slots slot-name) collect
       (if (listp type) (first type) type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/cc-set
;;; Name
;;; cc-set
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Modify global data stored in +cc-data+. For a list of possible
;;; keys see documentation of +cc-data+.
;;;
;;; Arguments
;;; key: The key to look for in +cc-data+
;;;
;;; Return Value
;;; The modified value associated with the given key.
;;;
;;; Example
#|
(cc-set :src-dir "/home/simon/comic/") --> "/home/simon/comic/"
|#
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-set (key value)
  (if (assoc key +cc-data+)
      (progn
	(rplacd (assoc key +cc-data+) value)
	(cc-get key))
      	(format t "WARNING: The key ~a could not be found in +cc-data+.~%~
                            No value was set." key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/cc-push
;;; Name
;;; cc-set
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Modify global data stored in +cc-data+. For a list of possible
;;; keys see documentation of +cc-data+.
;;;
;;; This function is a syntactic sugar, build upon cc-set and
;;; cc-get. Assuming that the value of a given key is a list
;;; (or nil), the new value will be pushed into the list. 
;;;
;;; Arguments
;;; value: The new value to push into the list.
;;; key: The key to look for in +cc-data+
;;;
;;; Return Value
;;; The new value
;;;
;;; Example
#|
(cc-set :src-dir "/home/simon/comic/") --> "/home/simon/comic/"
|#
;;;
;;; Last Modified
;;; 2020/01/30
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-push (value key)
  "Based on cc-get and cc-set, this is a short way to push a new item
   into a list stored in +cc-data+."
  (let ((old-val (cc-get key)))
    (unless (listp old-val)
      (cc-error 'CC-PUSH
	  "The value associated with key ~a is not of type list."
	key))
    (cc-set key (cons value old-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LOAD DEPENDENCIES                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun cc-load-dep-package (package-designator
;; 			    error-if-not-found
;; 			    &rest lisp-files-to-load)
;;   ;; The designated package will only be loaded if it is not yet
;;   ;; present!
;;   (unless (find-package package-designator)
;;     (loop for lisp-file in lisp-files-to-load do
;; 	 (let* ((warning-string
;; 		 (format nil "~&WHILE LOADING COMIC DEPENDENCY ~a:~
;;                               ~%the file ~a could not be found."
;; 			 package-designator lisp-file))
;; 		(lisp-file-in-cc-dir
;; 		 (concatenate 'string
;; 			      (cc-get :src-dir)
;; 			      lisp-file)))
;; 	   (cond ((probe-file lisp-file);check for absolute path
;; 		  (load lisp-file))
;; 		 ((probe-file lisp-file-in-cc-dir);check in cc-dir
;; 		  (load lisp-file-in-cc-dir))
;; 		 (error-if-not-found ;error
;; 		  (error warning-string))
;; 		 (t ;warning
;; 		  (format t warning-string)))))))

;; Load Common Music
;; (cc-load-dep-package :cm t "cm-2.6.0/src/cm.lisp")

;; ;; load clm
;; (cc-load-dep-package :clm t "/dep/clm5/all.lisp")

;; (in-package :clm)
;; (shadow '(sprectrum))
;; (in-package :comic)

;; (in-package :sc)
;; (shadow '(make-event))
;; (in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERRORS AND UNIT-TESTS                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/cc-reload
;;; Name
;;; cc-reload
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Reload the comic-package. Can be usefull to reset everything
;;; without heaving to restart lisp.
;;;
;;; Arguments
;;; None
;;;
;;; Return Value
;;; t
;;;
;;; Last Modified
;;; 2020/02/14
;;;
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cc-hard-reload ()
  "Reload the comic-package."
  (in-package :cl-user)
  (delete-package :comic)
  (load (concatenate 'string (cc-get :src-dir) "all.lsp"))
  (cl-user::cc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/when-verbose
;;; Name
;;; when-verbose
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Check if :verbose is set to t in +cc-data+. If so, return t,
;;; else nil. If a string-to-print is given, print it to std-out.
;;;
;;; Optional Arguments
;;; - string-to-print
;;; - format arguments: Arguments used in string-to-print in a
;;;   format-manner
;;;
;;; Return Value
;;; boolen
;;;
;;; Last Modified
;;; 2020/06/28
;;;
;;; Synopsis
(defun when-verbose (&optional string-to-print
		     &rest format-arguments)
;;; ****
  (when (cc-get :verbose)
    (when string-to-print
      (format t "~&")
      (apply #'format t string-to-print format-arguments)
      (format t "~%"))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc-error (in-context datum &body arguments)
  `(error
    (concatenate 'string
		 (format
		  nil
		  "--------COMIC--------~%~
                   ERROR IN ~S: " ,in-context)
		 (format
		  nil
		  ,datum
		  ,@arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-type-error (in-context variable expected-type)
  (cc-error in-context
       "Wrong type of value ~a:~%~
        Should be of type ~a, not ~a."
    variable expected-type
    (type-of variable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc-warn (in-context datum &rest arguments)
  `(cerror
    "Don't say I did not warn you!"
    (concatenate 'string
		 (format
		  nil
		  "--------COMIC--------~%~
                   WARNING IN ~S: " ,in-context)
		 (format
		  nil
		  ,datum
		  ,@arguments))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro cc-test (name &body forms)
  "Used to define a unit-test for comic"
  (format t "~&~%TESTING ~a... ~%~%" name)
  (loop for f in forms do
       (format t "~&    ~g " f)
       (if (eval f)
	   (format t "-> t")
	   (progn (format t "-> NIL")
		  (error
		   "--------COMIC--------~%~
                FAILED CC-TEST~%~
                name: ~s~%code: ~a~%"
		   name f))))
  (format t "~&~%~a: all tests passed!~%"
	  name)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-run-tests ()
  "Run all comic unit-tests in cc-tests.lsp"
  (let ((test-file
	 (concatenate 'string (cc-get :src-dir) "cc-tests.lsp")))
    (if (probe-file test-file)
	(load test-file)
	(cc-error 'cc-run-tests "The test-file 'cc-tests.lsp' ~
                                could not be found.")))
  t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-load-files (&rest filenames)
  (loop for filename in filenames do
    (load (merge-pathnames filename (cc-get :src-dir))))
  t)

(defun cc-load-compile-files (&rest filenames)
  (loop for filename in filenames do
       (load (compile-file (merge-pathnames filename (cc-get :src-dir)))))
  t)

(defun cc-load-files-if (condition &rest filenames)
  (when condition (apply #'cc-load-files filenames)))

;;; LOAD MODULES: MAIN / UTILITIES
(cc-load-files "utilities.lsp"
	       "classes.lsp"
	       "placed-object.lsp"
	       "envelopes.lsp"); functions for creating lists of numbers

;;; LOAD MODULE: EVENT
(cc-load-files
 "event.lsp"; event/comic classes and methods
 "comic.lsp"
 "event-transform.lsp"
 "superhero.lsp")

;;; LOAD I/O Functionality
(cc-load-files "read.lsp"
	       "write.lsp")

;;; LOAD MODULE: RENDER
(cc-load-files
 "prepare-render.lsp"
 "protagonist.lsp"
 "render-mode.lsp"
 "render.lsp"; the render-module
 "analyse.lsp"); the analysis-module for auto-creation of comics

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/add-software-path
;;; Name
;;; add-software-path
;;;
;;; File
;;; all.lsp
;;;
;;; Description
;;; Add an external software path to Comic. It will be stored in 
;;; +cc-data+ and is accessible via its name (a symbol). Using
;;; (run), a software can be called directly by that symbol, e.g. 
;;; when writing a render-mode.
;;;
;;; When setting up Comic, it is required to call this function
;;; for every program that is a dependency of a render-mode that
;;; one wants to use. It is not necassary to get every available
;;; render-mode up and running in order to use Comic, so the paths
;;; to add will be different for each individual project. The 
;;; render-function will signal an error when any dependency of 
;;; a render-mode given is not available.
;;;
;;; Arguments
;;; name: A symbol, the name of the program.
;;; path: An absolute path to the software. On a unix system, the
;;;       path to a program can be found using the whereis-command.
;;;
;;; Return Value
;;; boolean, indicating whether the specified path is a file.
;;; The function will not test if the program works properly or if
;;; it is executable at all.
;;;
;;; Example
#|
(add-software-path 'ffmpeg "/usr/bin/ffmpeg")
|#
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun add-software-path (name path)
;;; ****
  (when (probe-file path)
    (let* ((ext (cc-get :external-software)))
      (if (assoc name ext)
	  (progn
	    (rplacd (assoc name ext) path)
	    (cc-set :external-software ext))
	  (cc-set :external-software
		  (cons (cons name path) ext))))
    (push (make-keyword name) *features*)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-save-preferences ()
  (output-src-file "user.preferences"
		   (format nil
			   ";;; This is a preference-file for Comic. 
;;; You can load it into lisp using (cc-load-preferences)~%~%~a"
			   (to-code (cc-get :preferences)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-load-preferences ()
  ;; load preference file into +cc-data+
  (let* ((pref-file (src-dir "user.preferences"))
	 (exists? (when (probe-file pref-file) t)))
    (if exists?
	(cc-set :preferences
		(load-and-return pref-file))
	(format t "~&Comic user-preferences file not found.~%"))
    ;; update all:
    (when (cc-get :preferences) ;makes sure the file was not empty
      ;; add software-paths
      (loop for p in (cc-get :preferences :software-paths)
	    do (add-software-path (car p) (cdr p)))
      ;; (re-)load render-modes
      (apply #'cc-load-files (cc-get :preferences :render-mode-files))
      ;; set a few values:
      (loop for key in (list :external-program-call
			     :output-dir
			     :tmp-dir)
	    do
	       (cc-set key (cc-get :preferences key)))
      exists?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-change-preferences (&key
				(render-mode-files nil rmp)
				(software-paths nil spp)
				(output-directory nil odp)
				(tmp-directory nil tdp)
				(external-program-call nil epcp))
  ;; use set values if no new value is supplied
  (unless rmp
    (setq render-mode-files (cc-get :preferences :render-mode-files)))
  (unless spp
    (setq software-paths (cc-get :preferences :software-paths)))
  (unless odp
    (setq output-directory (cc-get :preferences :output-directory)))
  (unless tdp
    (setq tmp-directory (cc-get :preferences :tmp-directory)))
  (unless epcp
    (setq external-program-call
	  (cc-get :preferences :external-program-call)))
  ;; set preferences in +cc-data+
  (cc-set :preferences
	  (list 
	   (cons :render-mode-files render-mode-files) 
	   (cons :software-paths software-paths)
	   (cons :output-directory output-directory) 
	   (cons :tmp-directory tmp-directory) 
	   (cons :external-program-call 'SB-EXT:RUN-PROGRAM)))
  ;; save preferences to file user-preference.lisp
  (cc-save-preferences)
  ;; load the preferce-file back in to apply changes
  (cc-load-preferences))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/load-comic
;;; Name
;;; load-comic
;;;
;;; File
;;; load.lsp
;;;
;;; Description
;;; Loads the main part of comic after checking all external
;;; software. The standard way to load comic is:
;;; (load "/path-to-comic-src/all.lsp")
;;; (in-package :comic)
;;; (load-comic OPTIONS)
;;;
;;; All dependencies (lisp-packages and external software) must be
;;; loaded before calling load-comic. The all.lsp file may be loaded
;;; at anytime during the load-process.
;;;
;;; It is necessary to load comic in two steps in order to make sure
;;; that only the functionality is loaded that can be used with the
;;; given dependencies. Without any external software, most
;;; render-modes will not be loaded!
;;;
;;; Example
#|
(load-comic :path-to-ffmpeg "path/to/ffmpeg")
|#
;;;
;;; Last Modified
;;; 2020/11/07
;;; 
;;; Synopsis
(defun cc-soft-reload ()
;;; ****
  ;; initial call to the macro when loading comic:

  (when (find-package :cm)
    (cc-load-files "midi.lsp"))
  
;;; LOAD REST 0F MODULE: MAIN
  ;; units must be loaded here, as 
  (cc-load-files "unit.lsp"); loads "unit-functions.lisp"
  (when (find-package :clm)
    (cc-load-compile-files "clm-mix.lsp"
			   "clm-ins.lsp"))

  ;; load mcclim for gui-editor
  (when (find-package :clim)
    (cc-load-files "gui.lsp"))

  ;; add some fun!
  (cc-load-files "fun.lsp")

  ;;load preferences
  (cc-load-preferences)
  
;;; after load will print out status information: 
  (cc-load-files "after-load.lsp"))

(cc-soft-reload)

(defun editor (&optional comic)
  (unless (member :comic-gui-editor *features*)
    ;; try to load mcclim via quicklisp if it is not there:
    (unless (find-package :clim)
      (when (find-package :quicklisp)
	(ql:quickload :mcclim)))
    ;; load the gui file if we have mcclim it now:
    (when (find-package :clim)
      (cc-load-files "gui.lsp")))
  ;; ... if it is possible, start the editor
  (when (find-package :clim)
    (cc-start-gui-editor comic)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF comic/all.lisp
