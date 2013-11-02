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
  (plist-hash-table
   (list
    (lit "CalRGB") 3
    (lit "CalGray") 1
    (lit "Lab") 3
    (lit "DeviceRGB") 3
    (lit "DeviceCMYK") 4
    (lit "DeviceGray") 1
    (lit "Separation") 1
    (lit "Indexed") 1
    (lit "Pattern") 1)))
  
