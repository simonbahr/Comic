;;; This is a preference-file for Comic. 
;;; You can load it into lisp using (cc-load-preferences)


(list 
 (list :RENDER-MODE-FILES "rm-isis-mode.lsp" "rm-midi-mode.lsp"
 "rm-video-synthesizer.lsp" "rm-videofile-mode.lsp"
 "rm-soundfile-mode.lsp" "rm-sinewave-mode.lsp") 
 (list :SOFTWARE-PATHS 
       (cons 'FFMPEG "/usr/bin/ffmpeg") 
       (cons 'ISIS "/home/simon/Software/ISiS_V1.2.6/isis.sh")) 
 (cons :OUTPUT-dir "/home/simon/Schreibtisch") 
 (cons :TMP-DIR "/tmp/") 
 (cons :EXTERNAL-PROGRAM-CALL 'SB-EXT:RUN-PROGRAM))
