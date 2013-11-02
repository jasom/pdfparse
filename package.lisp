;;;; package.lisp

(defpackage #:pdfparse-device
  (:use #:cl)
  (:export
   :DEVICE-BEGIN-FIGURE
   :DEVICE-BEGIN-PAGE
   :DEVICE-BEGIN-TAG
   :DEVICE-DO-TAG
   :DEVICE-END-FIGURE
   :DEVICE-END-PAGE
   :DEVICE-END-TAG
   :DEVICE-PAINT-PATH
   :DEVICE-RENDER-IMAGE
   :DEVICE-RENDER-STRING
   :device-ctm))

(defpackage #:pdfparse
  (:use #:cl #:alexandria #:split-sequence #:pdfparse-device)
  (:export :kwd :lit :ps-base-parser :parser-nexttoken :ps-eof :ps-stack-parser
	   :parser-nextobject))

(defpackage #:cmap
  (:use #:cl)
  (:export #:get-cmap #:cmap #:cmap-not-found))
