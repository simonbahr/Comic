(in-package :comic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(make-comic
 '+test-comic+
 :title "A Comic on Comics"
 :subtitle "Generated using the Comic-Mode in Comic."
 :author "Simon Bahr"
 :events (loop-make-event
	    :amplitude 1
	    :events
	      (merge-ls
	       (loop for n to 10 collect
		    (make-event :pitch (* n 100)
				:amplitude (mod (/ n 10.0) 1)))
	       (loop-make-event
		  :location in (circular-list '((3 2) (8 9) (7 1)
						(0 9) (0 0) (0 0) (1 1)))
		  :expansion '(1 1)
		  :pitch by 10
		  :amplitude from 0 by 0.02
		  :duration in (circular-list '(3 12 100 2 1 3 91 2 1 48 22 37))
		  :text in
		    (string-to-list
		     "Comics is a medium used to express narratives or other ideas through images, usually combined with text. It typically takes the form of a sequence of panels of images. Textual devices such as speech balloons, captions, and onomatopoeia can indicate dialogue, narration, sound effects, or other information. The size and arrangement of panels contribute to narrative pacing. Cartooning and other forms of illustration are the most common image-making means in comics; fumetti is a form which uses photographic images. Common forms include comic strips, editorial and gag cartoons, and comic books. Since the late 20th century, bound volumes such as graphic novels, comic albums, and tankōbon have become increasingly common, while online webcomics have proliferated in the 21st century.

The history of comics has followed different paths in different cultures. Scholars have posited a pre-history as far back as the Lascaux cave paintings in France. By the mid-20th century, comics flourished, particularly in the United States, western Europe (especially France and Belgium), and Japan. The history of European comics is often traced to Rodolphe Töpffer's cartoon strips of the 1830s, but the medium truly became popular in the 1930s following the success of strips and books such as The Adventures of Tintin. American comics emerged as a mass medium in the early 20th century with the advent of newspaper comic strips; magazine-style comic books followed in the 1930s, in which the superhero genre became prominent after Superman appeared in 1938. Histories of Japanese comics and cartooning (manga) propose origins as early as the 12th century. Modern comic strips emerged in Japan in the early 20th century, and the output of comics magazines and books rapidly expanded in the post-World War II era (1945–) with the popularity of cartoonists such as Osamu Tezuka (Astro Boy, et al.). Comics has had a lowbrow reputation for much of its history, but towards the end of the 20th century began to find greater acceptance with the public and academics." ". " t)
		  :start-time from 3)
	       (list (make-event)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-random-comic ()
  "make a random comic structure without values"
  (labels ((dice () (when (= (random 6) 0) t))
	   (random-string (&optional ls)
	     (if (dice)
		 (apply #'concatenate 'string ls)
		 (random-string
		  (cons
		   (string
		    (char
		     "abcdefghijklmnopqrstuvwxyz"
		     (random 26)))
		   ls))))
	   ;;make a random event:
	   (random-event ()
	     (let ((args nil))
	       (when (dice)
		 (push (secs (random 300)) args)
		 (push :start-time args))
	       (when (dice)
		 (push (hz (+ 50 (random 950))) args)
		 (push :pitch args))
	       (when (dice)
		 (push (random 1.0) args)
		 (push :amplitude args))
	       (when (dice)
		 (push (random-string) args)
		 (push :text args))
	       (when (dice)
		 (push (list (- (random 20.0) 10)
			     (- (random 20) 10)
			     (- (random 20) 10))
		       args)
		 (push :location args))
	       (when (dice)
		 (push (list (- (random 20) 10)
			     (- (random 20) 10)
			     (- (random 20) 10))
		       args)
		 (push :expansion args))
	       (apply #'make-event args)))
	   ;;make a random length list of events:
	   (random-amount-events ()
	     (loop repeat (1+ (random 10)) collect (random-event)))
	   (helper (comic)
	     (doevents (e comic)
	       (unless (events e)
		 (when (and (dice) (dice))
		   (add-events e (random-amount-events)))))
	     (let ((amount (count-events comic)))
	       (if (> amount 100)
		   (if (null (events comic))
		       (make-random-comic);; (make-random-comic)
		       comic)
		   (helper comic)))))
    (let ((the-comic
	   (helper (make-comic 'Random-Comic))))
      (cc-assign-ids-to-events the-comic)
      the-comic)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((environments
       '(127745 127747 127748 127749 127750 127751 127752
	 127753 127754 127755 127756 127757 127784 127785
	 127758 127759 127780 127781 127782 127783 127786
	 127914 127958 127957 127956 127963 127961 127960
	 127959 127958 127957 127956 127968 127969 127970
	 127974 127983 127979 128332 128333 128641))
      (things
       (loop for n from 127789 to 127908 collect n))
      (people
       (append '(127938 127939 127940 127943 127947 127946
		 127948 128373 128378 128375)
	       (loop for n from 128000 to 128060 collect n)
	       (loop for n from 128102 to 128131 collect n)
	       (loop for n from 128512 to 128587 collect n))))
  (make-render-mode comic-mode :other
		    :optional-slots
		    (text amplitude pitch
			  start-time duration
			  expansion location)
		    :header-code
		    (setq tmp1 (format nil "<html>
  <head>
    <style>
      body {text-align:center;font-family:sans-serif;
	   }
      h1 {font-size: 500%;}
      #page {display: flex; flex-wrap: wrap;}
      .panel {margin: 12px; width:640px; height: 640px;
	      border: medium solid black;
	      position:relative;}
      .textbox {
	  position:absolute; left: 6px; top: 6px;
	  border: thin solid black;
	  background-color:#e4f573;
	  padding:6px;}
      .emoji {position:absolute;font-size:300px;bottom:0;left:10%
	     }
      .bubble {
	  position: relative;
	  font-family: arial;
	  font-size: 1.1em;
	  background: #ff8300;
	  color: #fff;
	  top: 20px;
	  left: 33%;
      }
      .speech {
	  border-radius: 10px;
	  padding: 20px;
	  max-width: 51%;
      }
      .speech-triangle::after {
	  content: '';
	  border: 20px solid transparent;
	  border-left: 20px solid #ff8300;
	  border-top: 20px solid #ff8300;
	  position: absolute;
	  bottom: -20px;
	  left: 20%;
      }
      .thought {
	  border-radius: 50%;
	  padding: 50px 70px;
	  max-width: 30%;
      }
      .circular1 {
	  background: #ff8300;
	  width: 15px;
	  padding: 15px;
	  border-radius: 50%;
	  position: absolute;
	  left: 10px;
	  bottom: -10px;
      }
      .circular2 {
	  background: #ff8300;
	  width: 10px;
	  padding: 10px;
	  border-radius: 50%;
	  position: absolute;
	  left: 0;
	  bottom: -35px;
      }
      .small-emoji {
	  position:absolute;
          text-align:right;
          bottom:0;
          width:100%;
      }
    </style>
  </head>
  <body>
    <h1>~a</h1>
    <h2>~a</h2>
    <div id='page'>
" (if title title "") (if subtitle subtitle ""))
			  tmp3 t)
		    :event-code
		    ;; CHOOSE EMOJY
		    (let* ((emoji-ls
			    (if (and text (> (length text) 0))
				(if (or
				     tmp3
				     (and start-time
					     (u< (u-mod start-time 10) 1))
					(> (length text) 160))
				    environments
				    people)
				things))
			   (fixed-random
			    (mod 
			     (* .01
				(sum
				 (loop for val in
				      (flat
				       (list
					pitch
					location
					expansion
				        start-time))
				    when val
				    collect (value val))))
			     1))
			   (emoji (nth (round
					(* fixed-random (length emoji-ls)))
				       emoji-ls)))
		      ;; make a new panel if text is given
		      (if (and text (> (length text) 0))
			  (setq
			   tmp1
			   (concatenate
			    'string tmp1
			    (with-output-to-string (html)
			      (format
			       html
			       "~&      <div class='panel' 
           style='width:~dpx;background-color:#~6,'0X;'>~%"
			       ;; width of panel
			       (if duration
				   (let ((dur (value duration)))
				     (cond ((< dur 1)
					    480)
					   ((< dur 100)
					    640)
					   (t 960)))
				   640)
			       ;; bkg-color
			       (if pitch
				   (round
				    (* (/ (mod (value pitch 'hz) 1000)
					 1000)
				      16777215))
				   16777215))
			      ;; small emojis
			      (unless (eq environments emoji-ls)
				(loop for se in tmp2 do
				     (format html se)))
			      (setq tmp2 nil)
			      ;; Text element
			      (let ((font-size-in-bubble
				     (if amplitude
					 (* (1+ (value amplitude 'amp))
					    22)
					 32)))
				(if (or
				     tmp3
				     (and start-time
					  (u< (u-mod start-time 10) 1))
				     (> (length text) 160))
				    (format html
					    "<div class='textbox'>~a</div>"
					    text)
				    (if (or (null (id event))
					    (= 0 (mod (id event) 3)))
					(format
					 html
					 "
	<div class='speech bubble' style='font-size:~dpx;'>
          <div class='circular1'></div>
	  <div class='circular2'></div>
	  ~a
	</div>" font-size-in-bubble text)
					(format html
						"
        <div class='speech bubble' style='font-size:~dpx;'>
         <div class='speech-triangle'></div>
	 ~a
	</div>" font-size-in-bubble text)))
				(format html
					"<div class='emoji'>&#~a;</div>"
					emoji)
				(format html "~&      </div>~%")))
			    (setq tmp3 nil)))
			  ;; if no text, add the current emojy to tmp2
			  (push (format
				 nil
				 "<div class='small-emoji' ~
                         style='font-size:~dpx;'>&#~a;</div>"
				 (if amplitude
				     (* (1+ (value amplitude 'amp))
					100)
				     150)
				 emoji)
				tmp2))
		      :footer-code
		      (setq return-file-path
			    (output-tmp-file
			     (format nil "~a-comic-mode.html" (name protagonist))
			     (concatenate'string
			      tmp1 (format nil "
   </div>
  <p>~a</p>
  </body>
</html>" (if (and author date) (concatenate 'string author ", " date) ""))))))))
