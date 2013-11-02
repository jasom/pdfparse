;;cmapdb.py

(in-package :cmap)

(define-condition cmap-error (error) ())

(defparameter *cmap-debug* 0)

(defclass cmap ()
  ((code2cid :initarg code2cid)))

(defmethod initialize-instance :after ((self cmap) &rest args)
  (declare (ignorable args))
  (with-slots (code2cid) self
    (unless code2cid
      (setf code2cid (make-hash-table)))))

(defmethod is-vertical ((self cmap))
  nil)

(defmethod use-cmap ((self cmap) cmap)
  (assert (typep cmap 'cmap))
  (labels ((copy (src dst)
	     (loop for k being the hash-keys of src
		  using (hash-value v)
		  when (hash-table-p v)
		  do
		  (let ((d (make-hash-table)))
		    (setf (gethash k dst) d)
		    (copy d v))
		  else
		  do (setf (gethash k dst) v))))
    (copy (slot-value self 'code2cid)
	  (slot-value cmap 'code2cid))))




    
       
