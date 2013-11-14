(defpackage :simple-pdf-device
  (:use :cl :pdfparse-device))

(in-package simple-pdf-device)

(defclass simple-pdf-device ()
  ((output-stream :initarg :output-stream)
   (last-y :initform -1d20)))

(defmethod DEVICE-BEGIN-TAG ((device simple-pdf-device) tag &optional props))
(defmethod DEVICE-END-TAG ((device simple-pdf-device)))
(defmethod DEVICE-DO-TAG ((device simple-pdf-device) tag &optional props))
(defmethod DEVICE-BEGIN-PAGE ((device simple-pdf-device) page ctm))
(defmethod DEVICE-END-PAGE ((device simple-pdf-device) page))
(defmethod DEVICE-BEGIN-FIGURE ((device simple-pdf-device) bbox matrix))
(defmethod DEVICE-END-FIGURE ((device simple-pdf-device)))
(defmethod DEVICE-PAINT-PATH ((device simple-pdf-device) graphicstate stroke fill evenodd path))
(defmethod DEVICE-RENDER-IMAGE ((device simple-pdf-device) stream))
(defmethod DEVICE-RENDER-STRING ((device simple-pdf-device) textstate seq)
(let ((y (car (last (slot-value textstate 'pdfparse::matrix)))))
    (when (> (abs (- (slot-value device 'last-y) y)) 3)
      (write-char #\Newline (slot-value device 'output-stream)))
    (setf (slot-value device 'last-y) y))
  (loop with font = (slot-value textstate 'pdfparse::font)
       with outs = (slot-value device 'output-stream)
     for (a b . r) = seq then r
       when font
     do (loop for cid in (pdfparse::font-decode font a)
	     for ch = (handler-case (pdfparse::to-unichr font cid)
			  (pdfparse::key-error () (format nil "<CID ~X>" cid)))
	     when ch
	     do (format outs "~a" 
			ch))
       ;do (write-sequence
	   ;(babel:octets-to-string 
	    ;(map-into (make-array (length a) :element-type '(unsigned-byte 8)) #'char-code a)
	    ;:errorp nil) (slot-value device 'output-stream))
       unless r return nil))
  ;(write-char #\Newline (slot-value device 'output-stream)))
  ;(format t "RENDER-STRING: ~S~%" seq))

(defmethod (setf device-ctm) (ctm (device simple-pdf-device)) ctm)
