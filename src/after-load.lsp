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
(in-package :comic)

;; shortcut-functions for entering comic after load.
;; prints information if core-dependencies were not found.
(defun cc ()
  (in-package :cc)
  (format t
	  "~&~%...........................................................
.................................................-*=*......
....................-..-.-.--.-..-.-............+=*........
......:-.......-.-:.+.-:.+.-:.+.-:.+.-:.+.--.:-=+..-+==+-..
......*===+...:==*-.:+:-.:+*:++++*+*:-:.+:=@@+:-:#WW@##WWW:
.......-+*###=:..+=**++++++==##++++@WWW#++#WW+..@WW.....++:
.............-+#@WWWW@=++++WWWW#+++@W@WW++*WW#+=WW=........
..-=@WWW#::--=WWW#**=@WW=++#WW#W=++WW=WW*++@WW++@W@......+*
.@WW=+-*WW@=+@WW++++++=WW=+=WW=@W=*W@*WW#++#WW*+=WW#.--::WW
=WW-....-.-**WW@+++++++WW@++WW@*WW=W@+@WW++*WW#++*@WWWWWW@:
#W@....-+==++#WW*+++++*WW#*+@WW**WWW#*#WW*++@W@++++**:.....
*WW+.....*WW**@WW#***#WW@***=WW=*=@#**+*+*++++++++*+.......
.=WW=-..=WW@=***@WWWWW@***************************==+-.....
..-#WWWWWW#+:#=**********************************..........
.*@@@#+..:#+.-+=***************************=##**=-.........
.:-..-*@=.....-:---+##=****************==*#:=:+@=-.........
................-=@#-+*:--:=*+**++==+........+@-:@@@+......
.............-#@@#-.........-..............................
...........................................................~%~%")
  
  #-(and (or ccl sbcl) (or linux darwin))
  (format
   t
   "~&~%WARNING: Comic is recommended to run on a linux or macOS system with sbcl.
To run comic with a lisp implementation other than sbcl or ccl, you 
need to specify the lisp-function for running external programs yourself.
You can set it using (cc-set :external-program-call FUNCTION-NAME).
You can also set the path to your plattforms auto-open-program using
(add-software-path 'open PATH).~%~%")
  #-clm
  (format
   t
   "~&Comic uses Common Lisp Music for some core functionality,
especially when it comes to writing soundfiles. It is 
strongly recommended to load Common Lisp Music before loading
comic. You can find the recent version here:
https://ccrma.stanford.edu/software/clm/~%~%")
  #-cm
  (format
   t
   "~&Comic uses Common Music for some core functionality,
especially when it comes to writing midi data. It is 
strongly recommended to load Common Lisp Music before loading
comic. The Lisp-version of Common Music is included in Michael
Edward's 'Slippery Chicken', which can be found here:
https://github.com/mdedwards/slippery-chicken~%~%")
  ;;   #-slippery-chicken
  ;;   (format
  ;;    t
  ;;    "~&Comic uses Slippery Chicken for some core functionality,
  ;; especially when it comes to writing musical notation. It is 
  ;; strongly recommended to load Slippery Chicken before loading
  ;; comic.~%~%")
  #-ffmpeg
  (format
   t
   "~&Comic uses FFMPEG for processing video. In order to make use of
that, you need to add FFMPEG using add-software-path, or specify the
path when calling load-comic.~%~%")
  (format t "~&The following render-modes were successfully loaded:
~{~a~^, ~}~%~%" (loop for rm in (cc::cc-get :render-modes)
		      collect (first rm)))
  (when (member :comic-gui-editor *features*)
    (format t "~&Hey, wanna use the gui-editor?! :) Just call (editor)~%~%"))
  (format t "~&Output Directory is: ~a" (cc::cc-get :output-dir))
  (format t "~&Temporary Directory is: ~a~%~%" (cc::cc-get :tmp-dir))
  t)

(defun comic () (cc))

(in-package :cl-user)
(defun comic () (cc::cc))
(defun cc () (cc::cc))


(format t "Comic was successfully loaded! Enter comic calling (cc) or
(comic) when in package cl-user.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF comic/after-load.lisp
