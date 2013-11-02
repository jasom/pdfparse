;;;; pdfparse.asd

(asdf:defsystem #:pdfparse
  :serial t
  :description "Describe pdfparse here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:split-sequence
               #:alexandria
               #:babel
	       #:parse-float
	       #:flexi-streams
	       #:deflate
	       #:cl-ppcre
	       #:ironclad)
  :components ((:file "package")
	       (:file "util")
	       (:file "stubs")
	       (:file "device")
	       (:file "pdfcolor")
               (:file "pdfparse")
	       (:file "cmap")))

