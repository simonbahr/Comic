;;;; comic.asd

(asdf:defsystem #:comic
  :description ""
  :author "Simon Bahr <mail@simonbahr.de>"
  :license  "Specify license here"
  :version "1.0"
  ;; :depends-on ("mcclim")
  :perform (load-op (o c)
		    (load (system-relative-pathname "comic" "src/all.lsp"))))
