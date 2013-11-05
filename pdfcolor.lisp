(in-package :pdfparse)

(defparameter +literal-device-gray+ (lit "DeviceGray"))
(defparameter +literal-device-rgb+ (lit "DeviceRGB"))
(defparameter +literal-device-cmyk+ (lit "DeviceCMYK"))

(defclass pdf-color-space ()
  ((name :initarg :name)
   (ncomponents :initarg :ncomponents)))

(defun make-pdf-color-space (name ncomponents)
  (make-instance 'pdf-color-space :name name :ncomponents ncomponents))

(defmethod print-object ((self pdf-color-space) stream)
  (with-slots (name ncomponents) self
  (format stream "#<PDF-COLOR-SPACE ~s ncomponents=~d>"  name ncomponents)))

(defparameter +predefined-colorspace+
  (alist-hash-table
   (mapcar (lambda (x)
	     (cons (car x) (make-pdf-color-space (car x) (cdr x))))
	   (list
	    (cons (lit "CalRGB") 3)
	    (cons (lit "CalGray") 1)
	    (cons (lit "Lab") 3)
	    (cons (lit "DeviceRGB") 3)
	    (cons (lit "DeviceCMYK") 4)
	    (cons (lit "DeviceGray") 1)
	    (cons (lit "Separation") 1)
	    (cons (lit "Indexed") 1)
	    (cons (lit "Pattern") 1)))))
  
