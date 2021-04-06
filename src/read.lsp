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
;; main/read.lsp                                                             2020 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-list-from-file (file)
  "Read first lisp-expression from a file, e.g. a list containing data."
  (with-open-file (file file)
    (read file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* main/load-and-return
;;; Name
;;; load-and-return
;;;
;;; File
;;; read.lsp
;;;
;;; Description
;;; Reads a lisp-expression from a file, evaluates it and returns the
;;; result. Used when loading a comic from a saved file, e.g. (save my-comic)
;;;
;;; Arguments
;;; file
;;;
;;; Return Value
;;; A saved object, e.g. a comic
;;;
;;; Example
#|
(save my-comic "/home/simon/my-comic.lsp") --> t
(load-and-return "/home/simon/my-comic.lsp") --> my-comic 
|#
;;;
;;; Last Modified
;;; 2021/01/22
;;;
;;; Synopsis
(defun load-and-return (file)
;;; ****
  "Reads a lisp-expression from a file, evaluates it and returns the
  result. Used when loading a comic from a saved file."
  (let ((form (read-list-from-file file)))
    (when form (eval form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-u4 (stream &optional position)
  "Reads a 4 byte little-endian integer from stream."
  (when position (file-position stream position))
  (let ((u4 0))
    (setf (ldb (byte 8 0) u4) (read-byte stream))
    (setf (ldb (byte 8 8) u4) (read-byte stream))
    (setf (ldb (byte 8 16) u4) (read-byte stream))
    (setf (ldb (byte 8 24) u4) (read-byte stream))
    u4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-u2 (stream &optional position)
  "Reads a 2 byte little-endian integer from stream."
  (when position (file-position stream position))
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte stream))
    (setf (ldb (byte 8 8) u2) (read-byte stream))
    u2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-wav-file-data (file)
  "Returns three values: The duration (in secs), samplerate and number
  of channels of a wav-file"
  (with-open-file (f file
		     :direction :input
		     :element-type '(unsigned-byte 8))
    (let* ((flen (file-length f))
	   (srate (read-u4 f 24))
	   (channels (read-u2 f 22))
	   (bits-per-samp (read-u2 f 34))
	   (sound-duration (/ flen
			      (/
			       (* srate channels
				  bits-per-samp)
			       8))))
      (values (secs sound-duration) srate channels))))
