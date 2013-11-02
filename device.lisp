(in-package :pdfparse-device)


(defgeneric DEVICE-BEGIN-TAG (device tag &optional props))
(defgeneric DEVICE-END-TAG (device))
(defgeneric DEVICE-DO-TAG (device tag &optional props))
(defgeneric DEVICE-BEGIN-PAGE (device page ctm))
(defgeneric DEVICE-END-PAGE (device page))
(defgeneric DEVICE-BEGIN-FIGURE (device bbox matrix))
(defgeneric DEVICE-END-FIGURE (device))
(defgeneric DEVICE-PAINT-PATH (device graphicstate stroke fill evenodd path))
(defgeneric DEVICE-RENDER-IMAGE (device stream))
(defgeneric DEVICE-RENDER-STRING (device textstate seq))
