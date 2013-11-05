(in-package :pdfparse)

(defun strip (string)
  (rstrip (lstrip string)))

(defun lstrip (string)
  (with-output-to-string (outs)
    (let ((s string))
       (loop for c across s
	    with begin = t
	    unless (and begin (member c '(#\Space #\Tab #\Newline #\Return)))
	    do (write-char c outs)
	    (when begin (setf begin nil))))))

(defun rstrip (string)
  (nreverse
   (with-output-to-string (outs)
     (let ((s (nreverse string)))
       (loop for c across s
	    with begin = t
	    unless (and begin (member c '(#\Space #\Tab #\Newline #\Return)))
	    do (write-char c outs)
	    (when begin (setf begin nil)))))))

(defun isspace (s)
  (etypecase s
    (character
     (member s '(#\Space #\Tab #\Newline #\Return)))
    (string
     (loop for c across s
	  unless (isspace c) return nil
	  finally (return t)))))

(defpackage ps-keyword)
(defpackage ps-literal)

(defparameter *ps-keyword-package* (find-package :ps-keyword))
(defparameter *ps-literal-package* (find-package :ps-literal))

(declaim (inline litf kwd))
(defun litf (name) (intern name *ps-literal-package*))
(defmacro lit (name)
  (if (stringp name)
      `(quote,(intern name (find-package :ps-literal)))
      `(litf ,name)))

(defun kwd (name) (intern name *ps-keyword-package*))

(defvar *strict* t)

(defparameter +keyword-proc-begin+ (kwd "{"))
(defparameter +keyword-proc-end+ (kwd "}"))
(defparameter +keyword-array-begin+ (kwd "["))
(defparameter +keyword-array-end+ (kwd "]"))
(defparameter +keyword-dict-begin+ (kwd "<<"))
(defparameter +keyword-dict-end+ (kwd ">>"))
(defparameter +matrix-identity+ (list 1 0 0 1 0 0))

(defun mult-matrix (m1 m2)
  (destructuring-bind
	((a1 b1 c1 d1 e1 f1) (a0 b0 c0 d0 e0 f0)) (list m1 m2)
      (list (+ (* a0 a1) (* c0 b1))     (+ (* b0 a1) (* d0 b1))
	    (+ (* a0 c1) (* c0 d1))     (+ (* b0 c1) (* d0 d1))
	    (+ (* a0 e1) (* c0 f1) e0)  (+ (* b0 e1) (* d0 f1) f0))))

(defun apply-matrix-norm (mt norm)
  (destructuring-bind (a b c d e f) mt
    (declare (ignore e f))
    (destructuring-bind (p q) norm
      (list (+ (* a p) (* c q))
	    (+ (* b p) (* d q))))))

(define-condition ps-eof (error) ())
(define-condition ps-type-error (error) ())
(define-condition key-error (error) ())

(defun in-dict (k h)
  (nth-value 1 (gethash k h)))
