;;;Testing McClim

;; (defpackage comic-gui
;;   (:use #:clim #:clim-lisp :comic)
;;   (:export #:start-comic-gui))

(in-package :comic)

(use-package 

(define-application-frame comic-app ()
  ;; New addition of a slot to the application frame which
  ;; defines a application-specific slot.

  ;; The slot is simply a number.
  ()

  ;; The rest of the application frame is unchanged.
  (:pointer-documentation t)
  (:menu-bar t)
  (:panes
   (app :application
	:height 900
	:width 900
	:display-function 'display-app
	:scroll-bars nil
	:background (make-rgb-color 0.9 0.9 0.9)
	:text-style (make-text-style :fix :roman 12))
   (sel :application
	:height 400
	:width 300
	:text-style (make-text-style :fix :roman 12)
	:display-function 'display-sel)
   (int :interactor
	:text-style (make-text-style :fix :roman 12)
	:height 200
	:width 300))
   (:layouts
    (default
     (horizontally ()
       (vertically ()
	 sel
	 int)
       app))))

(defparameter *the-comic* (cc::make-comic 'untitled))
(defparameter *selected-element* *the-comic*)

(defun display-app (frame pane)
  (declare (ignore frame))
  (cc::print-events *the-comic* pane))

(defun display-sel (frame pane)
  (declare (ignore frame))
  (loop for slot in (cc::get-slots *selected-element*)
	do
	   (format pane "~&~a: ~a"
		   slot
		   (slot-value *selected-element* slot))))



(define-comic-app-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-comic-app-command (com-update :name t) ()
  t)

(define-comic-app-command (com-eval :name t) ((expr 'string))
  (eval (read-from-string expr)))

;;; PACKAGE PROBLEMS (SYMBOL SLOT_NAME IST IN CL USER DEFINIERT...)
(define-comic-app-command (com-set-slot :name t)
    ((slotname 'string) (value 'string))
  (setf (slot-value *selected-element*
		    (read-from-string (format nil "cc::~a" slotname)))
	(read-from-string value)))

(in-package :cc)
(defun start-comic-gui ()
  (let ((frame (comic-gui::make-application-frame 'comic-app)))
    (values frame
	    (clim-sys:make-process
	     (lambda ()
	       (comic-gui::run-frame-top-level frame))))))
