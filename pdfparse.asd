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
	       #:nibbles
	       #:deflate
	       #:cl-ppcre
	       #:ironclad)
  :components ((:file "package")
	       (:file "util")
	       (:file "metrics")
	       (:file "stubs")
	       (:file "device")
	       (:file "glyphlist")
	       (:file "encodingdb")
	       (:file "pdfcolor")
               (:file "pdfparse")
	       ))

