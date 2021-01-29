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
;; main/write.lsp                                                            2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/add-output-type
;;; Name
;;; add-output-type
;;;
;;; File
;;; write.lsp
;;;
;;; Description
;;; Add an output-type to the list of available output types.
;;; Choose the default file format.
;;;
;;; Arguments
;;; name: Name of output-type. A keyword
;;; format: file extension as symbol
;;;
;;; Return Value
;;; (cons name format)
;;;
;;; Example
#|
(add-output-type :sound wav)
|#
;;;
;;; Last Modified
;;; 2020/02/10
;;;
;;; Synopsis
(defun add-output-type (name)
;;; ****
  (cc-push name :output-types)
  name)
  ;; (cc-set :output-types (acons name format (cc-get :output-types)))
  ;; (cons name format))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;; Not sure if this is needed...
;; (defun get-output-type-suffix (output-type)
;;   (loop for (type suffix) in (cc-get :output-types)
;;      do
;;        (when (eq type output-type) (return suffix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cc-concat-paths (&rest strings)
  ;; convert to strings
  (setq strings
	(loop for s in strings collect
	     (cond ((stringp s)
		    s)
		   (t (format nil "~s" s)))))
  ;; kill all leading slashes in cdr
  (setq strings
	(cons (car strings)
	      (loop for path in (cdr strings) collect
		   (if (string-equal (subseq path 0 1) "/")
		       (subseq path 1)
		       path))))
  ;; ensure slashes at end
  (setq strings
	(append
	 (loop for path in (butlast strings) collect
	      (let ((len (length path)))
		(if (string-equal (subseq path (1- len)) "/")
		    path
		    (concatenate 'string path "/"))))
	 (last strings)))
  (apply #'concatenate 'string strings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun src-dir (pathspecs)
  "return the absolute path to a relativ path in source-directory"
  (cc-concat-paths (cc-get :src-dir) pathspecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/absolute-path
;;; Name
;;; absolute-path
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Return an absolute path to a given relative path in the current
;;; project directory.
;;;
;;; Arguments
;;; relative-path (string)
;;;
;;; Return Value
;;; absolute path (string)
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun absolute-path (relative-path)
;;; ****
  "return the absolute path to a relativ path in current project"
  (let ((current-project-name (cc-get :current-project-name))
	(current-project-path
	  ;; during render, this will hold the project path:
	  (if (cc-get-render-data 'output-dir)
	      (cc-get-render-data 'output-dir)
	      ;; if no project path is set during render, the default
	      ;; can be found in +cc-data+:
	      (cc-get :output-dir))))
    (when current-project-name
      (setq relative-path (cc-concat-paths
			   current-project-name
			   relative-path)))
    (cc-concat-paths
     current-project-path
     relative-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/tmp-path
;;; Name
;;; tmp-path
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Return an absolute path to a given relative path in the set
;;; tmp-directory. The tmp-dir is stored in +cc-data+, defaults to
;;; "/tmp" and can be changed using (cc-set :tmp-dir "new-tmp-dir").
;;;
;;; The tmp-dir is used in render-modes to store temporary files. 
;;; All files written by Comic to the tmp-dir will usually be removed
;;; after render (when key-arg :tidy-up is t). The tmp-dir can also
;;; be cleaned manually using delete-temporary-files. Files not 
;;; created using 
;;; 
;;; Arguments
;;; relative-path (string)
;;;
;;; Return Value
;;; absolute path (string)
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun tmp-path (relative-path)
;;; ****
  "return the absolute path to a relativ path in tmp dir"
  (cc-concat-paths
   (cc-get :tmp-dir)
   relative-path))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/output-file
;;; Name
;;; output-file
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Output a file to the current project-directory. If the file
;;; already exists, it will be overwritten. All necassary 
;;; directories will be created.
;;; 
;;; Arguments
;;; relative-path (string or pathname)
;;; contents (string)
;;;
;;; Return Value
;;; t
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun output-file (relative-path contents)
;;; ****
  "output a file to project directory"
  (with-open-file
      (file (ensure-directories-exist
	     (absolute-path relative-path))
	    :direction :output
	    :if-exists :supersede
	    :if-does-not-exist :create)
    (format file contents))
  t)

(defun output-src-file (relative-path contents)
  "output a file to source directory"
  (with-open-file
      (file (ensure-directories-exist
	     (src-dir relative-path))
	    :direction :output
	    :if-exists :supersede
	    :if-does-not-exist :create)
    (format file contents))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/output-tmp-file
;;; Name
;;; output-tmp-file
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Output a file to the current tmp-directory. If the file
;;; already exists, it will be overwritten. All necassary 
;;; directories will be created.
;;;
;;; Files written to the temporary directory using this function
;;; will be remembered (in +cc-data+ -> :temporary-files) and
;;; can all be deleted at once using delete-temporary-files.
;;; This will usually be done automatically after each render.
;;; 
;;; Arguments
;;; relative-path (string or pathname)
;;; contents (string)
;;;
;;; Return Value
;;; absolute path to file
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun output-tmp-file (relative-path contents)
;;; ****
  "output a file to temp directory"
  (let ((path (cc-concat-paths (cc-get :tmp-dir) relative-path)))
    (with-open-file
	(file (ensure-directories-exist path)
	      :direction :output
	      :if-exists :supersede
	      :if-does-not-exist :create)
      (format file contents))
    (cc-push path :temporary-files)
    path))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/make-tmp-file
;;; Name
;;; make-tmp-file
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Output a file to the current tmp-directory. All necassary 
;;; directories will be created. The filename will be auto-generated
;;; if no name is supplied.
;;;
;;; Files written to the temporary directory using this function
;;; will be remembered (in +cc-data+ -> :temporary-files) and
;;; can all be deleted at once using delete-temporary-files.
;;; This will usually be done automatically after each render.
;;;
;;; When no contents is given, this function can be used to only
;;; create a unique filename in the tmp-dir, writing the actual
;;; file in a different process, e.g. by an external program.
;;;
;;; Optional Arguments
;;; contents (string): May be supplied, but is optional. 
;;; type: The suffix of the file. Defaults to "tmp".
;;; name: Can be used to bypass the functions ability to
;;;       create a name automatically
;;;
;;; Return Value
;;; the absolute path to the file
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun make-tmp-file (&optional contents (type "tmp") name)
  "Output a file to temp directory and return its auto-generated name.
   If a name is supplied and the file exists, it will be overwritten."
;;; ****
  (labels ((make-filename ()
	     (let ((name (format nil "~a.~a"
				 (if name
				     name
				     (+ 100000 (random 100000)))
				 type)))
	       (if (and (not name)
			(probe-file
			 (cc-concat-paths (cc-get :tmp-dir) name)))
		   (make-filename)
		   name))))
    (let ((filename (ensure-directories-exist
		     (cc-concat-paths (cc-get :tmp-dir)
				      (make-filename)))))
      (when contents
	(with-open-file
	    (file filename
		  :direction :output
		  :if-exists :supersede
		  :if-does-not-exist :create)
	  (format file contents)))
      (cc-push filename :temporary-files)
      filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/delete-temporary-files
;;; Name
;;; delete-temporary-files
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;;
;;; Files written to the temporary directory using make-tmp-file or
;;; output-tmp-file will be remembered (in +cc-data+ -> :temporary-
;;; files) and can all be deleted at once using this function.
;;; This will usually be done automatically after each render (When
;;; :tidy-up is t), but may in some cases be usefull to do manually.
;;; (E.g. when tidy-up is set to nil in order to look at the output
;;; of a render-mode after render.)
;;; 
;;; Arguments
;;; None
;;;
;;; Return Value
;;; t
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun delete-temporary-files ()
;;; ****
  (loop for file in (cc-get :temporary-files) do
       (when (probe-file file)
	 (delete-file file)))
  (cc-set :temporary-files nil)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun probe-file-with-suffix (pathspecs suffix)
  "Checks if a file exists and has the specified file-extension."
  (if (and (probe-file pathspecs)
	   (> (length pathspecs) (length suffix))
	   (string-equal
	    pathspecs suffix
	    :start1 (- (length pathspecs) (length suffix))))
      (probe-file pathspecs)
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cp-file (from-file to-file &optional overwrite)
  (when (and (not overwrite) (probe-file to-file))
    (cc-warn 'CP-FILE "The target-file ~a already exists.~%~
                           Do you want to overwrite it?"
	     to-file))
  (if (probe-file from-file)
      (with-open-file (input-stream from-file
				    :direction :input
				    :element-type '(unsigned-byte 8))
	(with-open-file (output-stream (ensure-directories-exist to-file)
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create
				       :element-type '(unsigned-byte 8))
	  (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
	    (loop for pos = (read-sequence buf input-stream)
	       while (plusp pos)
	       do (write-sequence buf output-stream :end pos))))
	t)
      (format t "The file ~a does not exists. Nothing copied."
	      from-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/cp-to-project-dir
;;; Name
;;; cp-to-project-dir
;;; 
;;; File
;;; output.lsp
;;;
;;; Description
;;; Copies a file to the current project-directory. This is usually
;;; done when copying a file generated in tmp-dir to the project.
;;; 
;;; Arguments
;;; file (path to an existing file to copy)
;;;
;;; Optional Arguments
;;; overwrite (default: t), indicates whether an existing file in
;;; project-dir should be overwritten. If set to nil, Comic will
;;; signal a warning and ask, whether the file should be overwritten
;;; if it already exists.
;;; sub-dir (string): when set, the file is copied to a subdirectory
;;; in the project directory.
;;;
;;; Return Value
;;; t if file was copied, else nil.
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun cp-to-project-dir (file &optional (overwrite t) sub-dir)
;;; ****
  (let ((path (probe-file file)))
    (if path
	(let* ((fname (file-namestring path))
	       (new-path
		 (absolute-path
		  (if sub-dir
		      (format nil "~a/~a" sub-dir fname)
		      fname))))
	  (cp-file file new-path overwrite)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/run
;;; Name
;;; run
;;;
;;; File
;;; output.lsp
;;;
;;; Description
;;; Run an external software using the implementation-dependend
;;; function that is stored in +cc-data+ by :external-program-call.
;;; The default function name is sb-ext:run-program, for sbcl.
;;; 
;;; Arguments
;;; software-name: A key stored in +cc-data+ / :external-software.
;;; args (&rest): All arguments the call needs. Must be strings
;;; without spaces, where a space in the argument list is indicated
;;; by a new string.
;;;
;;; Return Value
;;; nil
;;;
;;; Last Modified
;;; 2020/06/23
;;; 
;;; Synopsis
(defun run (software-name &rest args)
;;; ****
  (when-verbose "~&Calling external program ~a~%" software-name)
  (funcall (cadr (cc-get :external-program-call))
	   (if (member software-name (flat (cc-get :external-software)))
	       (cc-get :external-software software-name)
	       software-name)
	   args :output (when (when-verbose) *standard-output*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun run-and-return-output (software-name &rest args)
;;; ****
  (when-verbose "~&Calling external program ~a~%" software-name)
  (with-output-to-string (out)
    (funcall (cadr (cc-get :external-program-call))
	     (if (member software-name (flat (cc-get :external-software)))
		 (cc-get :external-software software-name)
		 software-name)
	     args :output out)
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric mix (output-type
		 filepath-without-extension
		 files-or-content
		 &key &allow-other-keys))

(defmethod mix (output-type
		filepath-without-extension
		files-or-content
		&key print
		&allow-other-keys)
  (when print
    (format t
	    "~&No mixing method specified for output-type ~a. ~
             Copying files to project-directory."
	    output-type))
  (loop for file in files-or-content
	do
	   (when (probe-file file)
	     (cp-to-project-dir
	      file t
	      (file-namestring filepath-without-extension)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric to-code (object))

;; default for any object, including clos-instances:
(defmethod to-code (object)
  (let* ((slots (get-slots object))
	 (type (type-of object)))
    (if slots
	(format
	 nil
	 "(make-instance '~a~{ ~a~})~%"
	 type
	 (loop for slot in slots
	       collect
	       (format nil "~%:~a ~a"
		       slot
		       (to-code (slot-value object slot)))))
	(format nil "~a" object))))


;; for standard lisp objects:
(defmethod to-code ((object list))
  (if (single-cons-cell-p object)
      (format nil "~%(cons ~a ~a)"
	      (to-code (car object))
	      (to-code (cdr object)))
      (format nil "~%(list~{ ~a~})"
	      (loop for obj in object
		    collect
		    (to-code obj)))))

(defmethod to-code ((object symbol))
  (let ((cc-pkg (find-package :comic))
	(sym-pkg (symbol-package object)))
    (if (eq cc-pkg sym-pkg)
	(format nil "'~a" object)
	(format nil "'~a:~a" (package-name sym-pkg) object))))
    
(defmethod to-code ((object string))
  (format nil "\"~a\"" object))


;; and for frequently used comic-stuff:
(defmethod to-code ((object comic))
  (let* ((slots (get-slots object)))
    (format nil "(make-comic '~a ~{ ~a~})~%"
	    (name object)
	    (loop
	      for slot in slots
	      for val = (slot-value object slot)
	      unless (or (eq slot 'name) (not val))
		collect
		(format nil ":~a ~a"
			slot
			(to-code val))))))

(defmethod to-code ((object event))
  (let* ((slots (get-slots object)))
    (format nil "(make-event ~{ ~a~})~%"
	    (loop
	      for slot in slots
	      for val = (slot-value object slot)
	      when val
		collect
		(format nil ":~a ~a"
			slot
			(to-code val))))))

(defmethod to-code ((object null)) "NIL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric save (object file &key overwrite))

(defmethod save (object file &key (overwrite t))
  (with-open-file (file file :direction :output
			     :if-does-not-exist :create
			     :if-exists (if overwrite :supersede nil))
    (format file ";; This file was automatically created by COMIC and contains ~%~
                  ;; a saved ~a. Simply load the file back in to recreate~%~
                  ;; the object~%~%~a"
	    (type-of object)
	    (to-code object)))
  t)

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
