(in-package :pdfparse)
(defclass stub-font () ())

(defun make-pdf-type1-font (spec) (declare (ignore spec))(make-instance 'stub-font))
(defun make-pdf-true-type-font (spec) (declare (ignore spec))(make-instance 'stub-font))
(defun make-pdf-type3-font (spec) (declare (ignore spec))(make-instance 'stub-font))
(defun make-pdf-cid-font (spec) (declare (ignore spec))(make-instance 'stub-font))
