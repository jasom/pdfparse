;;;; pdfparse.lisp


;;; "pdfparse" goes here. Hacks and glory await!
(in-package #:pdfparse)

(defvar +matrix-identity+)

(declaim (optimize (speed 0) (debug 3)))


;;psparse.py

(defun 8bitstring-to-stream (string)
  (flexi-streams:make-in-memory-input-stream
   string
   :transformer #'char-code))
  

(defun keyword-name (x)
  (assert (eql (symbol-package x) *ps-keyword-package*))
  (symbol-name x))


(defun literal-name (x)
  (assert (eql (symbol-package x) *ps-literal-package*))
  (symbol-name x))

(defun pythonic-read (stream elements)
  (let* ((list (make-list elements))
	 (position (read-sequence list stream)))
    (coerce (subseq list 0 position) 'string)))

(defmacro str-append (place string)
  `(setf ,place
	 (concatenate 'string
		      ,place
		      ,string)))

;##  PSBaseParser
;##
(defparameter +EOL+ (ppcre:create-scanner "[\\r\\n]"))
(defparameter +SPC+ (ppcre:create-scanner "\\s"))
(defparameter +NONSPC+ (ppcre:create-scanner "\\S"))
(defparameter +HEX+ (ppcre:create-scanner "[0-9a-fA-F]"))
(defparameter +END-LITERAL+ (ppcre:create-scanner "[#/%\\[\\]()<>{}\\s]"))
(defparameter +END-HEX-STRING+ (ppcre:create-scanner "[^\\s0-9a-fA-F]"))
(defparameter +HEX-PAIR+ (ppcre:create-scanner "[0-9a-fA-F]{2}|."))
(defparameter +END-NUMBER+ (ppcre:create-scanner "[^0-9]"))
(defparameter +END-KEYWORD+ (ppcre:create-scanner "[#/%\\[\\]()<>{}\\s]"))
(defparameter +END-STRING+ (ppcre:create-scanner "[()\\134]"))
(defparameter +OCT-STRING+ (ppcre:create-scanner "[0-7]"))
(defparameter +ESC-STRING+ '((#\b . 8) (#\t . 9) (#\n . 10) (#\f . 12) (#\r . 13) (#\( . 40) (#\) . 41) (#\\ . 92) ))

(defconstant +ps-buf-size+ 4096)
(defparameter *ps-base-parser-debug* 0)
(defparameter *pdf-document-debug* 0)

(defclass ps-base-parser ()
  ((fp :initarg :fp)
   bufpos
   buf
   charpos
   paren
   hex
   oct
   %parse1
   %curtoken
   %curtokenpos
   %tokens
   %parse-main)
  (:documentation
  "Most basic PostScript parser that performs only tokenization.")
  )
(defgeneric (setf parser-document) (doc self))
(defgeneric device-ctm (self))
(defgeneric (setf device-ctm) (val self))
(defgeneric parser-flush (self))
(defgeneric parser-close (self))
(defgeneric parser-tell (self))
(defgeneric parser-poll (self &optional pos n))
(defgeneric parser-seek (self pos))
(defgeneric parser-fillbuf (self))
(defgeneric parser-nextline (self))
(defgeneric parser-revreadlines (self))
(defgeneric %parse-main (self s i))
(defgeneric %add-token (self obj))
(defgeneric %parse-comment (self s i))
(defgeneric %parse-literal (self s i))
(defgeneric %parse-literal-hex (self s i))
(defgeneric %parse-number (self s i))
(defgeneric %parse-float (self s i))
(defgeneric %parse-keyword (self s i))
(defgeneric %parse-string (self s i))
(defgeneric %parse-wclose (self s i))
(defgeneric %parse-wopen (self s i))
(defgeneric %parse-hexstring (self s i))
(defgeneric parser-add-results (self &rest objs))
(defgeneric get-filters (self))

(defmethod initialize-instance :after ((self ps-base-parser) &rest r)
  (declare (ignore r))
  (parser-seek self 0))

(defmethod parser-flush ((self ps-base-parser)) nil)

(defmethod parser-close ((self ps-base-parser))
  (parser-flush self))

(defmethod parser-tell ((self ps-base-parser))
  (+
   (slot-value self 'bufpos)
   (slot-value self 'charpos)))

(defmethod parser-poll ((self ps-base-parser) &optional pos (n 80))
  (let ((pos0 (file-position (slot-value self 'fp)))
	(pos (or pos (parser-tell self))))
    (file-position (slot-value self 'fp) pos)
    (pythonic-read (slot-value self 'fp) n)
    (file-position (slot-value self 'fp) pos0)))

(defmethod parser-seek ((self ps-base-parser) pos)
  "Seeks the parser to the given position."
  (when (<= 2 *ps-base-parser-debug*)
    (format  *error-output* "Seek: ~S" pos))
  (file-position (slot-value self 'fp) pos)
  (setf (slot-value self 'bufpos) pos
        (slot-value self 'buf) ""
        (slot-value self 'charpos) 0
        (slot-value self '%parse1) #'%parse-main
        (slot-value self '%curtoken) ""
        (slot-value self '%curtokenpos) 0
        (slot-value self '%tokens) ()))

(defmethod parser-fillbuf ((self ps-base-parser))
  (unless
      (< (slot-value self 'charpos)
	 (length (slot-value self 'buf)))
    (setf (slot-value self 'bufpos) (file-position (slot-value self 'fp))
	  (slot-value self 'buf)
	  (pythonic-read (slot-value self 'fp) +ps-buf-size+))
    (when (string= (slot-value self 'buf) "")
      (error (make-condition 'ps-eof)))
    (setf (slot-value self 'charpos) 0)))

(defmethod parser-nextline ((self ps-base-parser))
  "Fetches a next line that ends either with \\r or \\n."
  (let ((linebuf "")
	(linepos (parser-tell self))
	(eol nil))
    (loop
       (parser-fillbuf self)
       (when eol
	 (let ((c
		(aref (slot-value self 'buf)
		      (slot-value self 'charpos))))
	   (when (equal c #\Newline)
	     (setf linebuf (concatenate 'string
					linebuf
					(list c)))
	     (incf (slot-value self 'charpos)))
	   (return)))
       (let
	   ((end (nth-value 1
			  (ppcre:scan +EOL+
				      (slot-value self 'buf)
				      :start (slot-value self 'charpos)))))
	 (if end 
	     (progn
	       (setf linebuf
		     (concatenate 'string
				  linebuf
				  (subseq (slot-value self 'buf)
					  (slot-value self 'charpos)
					  end))
		     (slot-value self 'charpos) end)
	       (if
		(char= (aref (slot-value self 'buf)
			     (1- end))
		       #\Return)
		(setf eol t)
		(return)))
	     (setf linebuf
		   (concatenate 'string
				linebuf
				(subseq
				 (slot-value self 'buf)
				 (slot-value self 'charpos)))
		   (slot-value self 'charpos)
		   (length (slot-value self 'buf))))))
    (when (<= 2 *ps-base-parser-debug*)
      (format *error-output* "nextline: ~s" (list linepos linebuf)))
	(cons linepos linebuf)))


(defmethod parser-revreadlines ((self ps-base-parser))
  "Fetches a next line backword.
   This is used to locate the trailers at the end of a file."
  (let ((pos)
	(buf)
	(s)
	(n)
	(prevpos)
	(started nil))
    (lambda ()
      (block outer
	(tagbody
	   (when (not started)
	     (file-position (slot-value self 'fp)
			    (1- (file-length (slot-value self 'fp))))
	     (setf pos (file-position (slot-value self 'fp))
		   buf ""
		   started t)
	     (go start))
	   (loop
	      (setf s (subseq s 0 n)
		    buf ""
		    n (max
		       (position #\Return s :from-end t)
		       (position #\Newline s :from-end t)))
	      (when (not n)
		(setf buf (concatenate 'string
				       s
				       buf))
		(return))
	      (return-from outer
		(concatenate 'string
			     (subseq s n) buf)))
	 start
	   (loop while (< 0 pos)
	      do
		(setf prevpos pos
		      pos (max 0 (- pos +ps-buf-size+)))
		(file-position (slot-value self 'fp) pos)
		(setf s
		      (pythonic-read (slot-value self 'fp)
				     +ps-buf-size+))
		(when (string= s "") (return))
		(loop
		   (setf n (max (position #\Return s :from-end t)
				(position #\Newline s :from-end t)))
		   (when (not n)
		     (setf buf (concatenate 'string
					    s
					    buf))
		     (return))
		   (return-from outer
		     (concatenate 'string
				  (subseq s n) buf)))
		finally (return nil)))))))
					
(defmethod %parse-main ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +nonspc+ s :start i))
	 (c (and j (aref s j))))
    (if (not c)
	(length s)
	(progn
	  (setf (slot-value self '%curtokenpos)
		(+ j (slot-value self 'bufpos)))
	  (case c
	    (#\%
	     (setf (slot-value self '%curtoken) "%"
		   (slot-value self '%parse1)
		   #'%parse-comment)
	     (1+ j))
	    (#\/
	     (setf (slot-value self '%curtoken) ""
		   (slot-value self '%parse1)
		   #'%parse-literal)
	     (1+ j))
	    ((#\+ #\- #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0)
	     (setf (slot-value self '%curtoken) (string c)
		   (slot-value self '%parse1)
		   #'%parse-number)
	     (1+ j))
	    (#\.
	     (setf (slot-value self '%curtoken) (string c)
		   (slot-value self '%parse1)
		   #'%parse-float)
	     (1+ j))
	    (#.(coerce "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
		       'list)
	       (setf (slot-value self '%curtoken) (string c)
		     (slot-value self '%parse1)
		     #'%parse-keyword)
	       (1+ j))
	    (#\(
	     (setf (slot-value self '%curtoken) ""
		   (slot-value self 'paren) 1
		   (slot-value self '%parse1)
		   #'%parse-string)
	     (1+ j))
	    (#\<
	     (setf (slot-value self '%curtoken) ""
		   (slot-value self '%parse1)
		   #'%parse-wopen)
	     (1+ j))
	    (#\>
	     (setf (slot-value self '%curtoken) ""
		   (slot-value self '%parse1)
		   #'%parse-wclose)
	     (1+ j))
	    (t
	     (%add-token self (kwd (string c)))
	     (1+ j)))))))

(defmethod %add-token ((self ps-base-parser) obj)
  (push (cons (slot-value self '%curtokenpos) obj)
	(slot-value self '%tokens)))
  
(defmethod %parse-comment ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +eol+ s :start i)))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (if (not j)
	(length s)
	(progn
	  (setf (slot-value self '%parse1) #'%parse-main)
	  j))))
	
(defmethod %parse-literal ((self ps-base-parser) s i)
  (let ((j (ppcre:scan +end-literal+ s :start i)))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (cond
      ((not j)
       (length s))
      ((char= (aref s j) #\#)
       (setf (slot-value self 'hex) ""
	     (slot-value self '%parse1) #'%parse-literal-hex)
       (1+ j))
      (t
       (%add-token self (lit (slot-value self '%curtoken)))
       (setf (slot-value self '%parse1) #'%parse-main)
       j))))

(defmethod %parse-literal-hex ((self ps-base-parser) s i)
  (let ((c (string (aref s i))))
    (cond
      ((and (ppcre:scan +hex+ (string c))
	    (< (length (slot-value self 'hex)) 2))
       (setf (slot-value self 'hex)
	     (concatenate 'string (slot-value self 'hex) c))
       (1+ i))
      (t
       (when (string/= (slot-value self 'hex) "")
	 (setf (slot-value self '%curtoken)
	       (concatenate 'string
			    (slot-value self '%curtoken)
			    (list
			    (code-char
			     (parse-integer (slot-value self 'hex)
					    :radix 16))))))
       (setf (slot-value self '%parse1) #'%parse-literal)
       i))))

(defmethod %parse-number ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +end-number+ s :start i))
	 (c (and j (aref s j))))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (cond
      ((not c)
       (length s))
      ((char= c #\.)
       (setf (slot-value self '%curtoken)
	     (concatenate 'string
			  (slot-value self '%curtoken)  
			  (list c))
	     (slot-value self '%parse1) #'%parse-float)
       (1+ j))
      (t
       (ignore-errors
	 (%add-token self
		     (parse-integer (slot-value self '%curtoken)
				    :radix 10)))
       (setf
	(slot-value self '%parse1) #'%parse-main)
       j))))

(defmethod %parse-float ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +end-number+ s :start i)))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (if (not j)
	(length s)
	(progn
	  (ignore-errors
	    (%add-token self
			(parse-float:parse-float
			 (slot-value self '%curtoken) :type 'double-float)))
	  (setf (slot-value self '%parse1)
		#'%parse-main)
	  j))))
	
(defmethod %parse-keyword ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +end-keyword+ s :start i)))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (if (not j)
	(length s)
	(progn
	  (cond
	    ((string= (slot-value self '%curtoken)
		      "true")
	     (%add-token self t))
	    ((string= (slot-value self '%curtoken)
		      "true")
	     (%add-token self nil))
	    (t
	     (%add-token self
			 (kwd (slot-value self '%curtoken)))))
	  (setf (slot-value self '%parse1)
		#'%parse-main)
	  j))))
	     
(defmethod %parse-string ((self ps-base-parser) s i)	    
  (let* ((j (ppcre:scan +end-string+ s :start i))
	 (c (and j (aref s j))))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (case c
      ((nil)
       (length s))
      (#\\
       (setf (slot-value self 'oct) ""
	     (slot-value self '%parse1) #'%parse-string-1)
       (1+ j))
      (#\(
       (incf (slot-value self 'paren))
       (setf (slot-value self '%curtoken)
	     (concatenate 'string
			  (slot-value self '%curtoken)
			  (list c)))
       (1+ j))
      (#\)
       (decf (slot-value self 'paren))
       (if (/= (slot-value self 'paren) 0)
	   (progn
	     (setf (slot-value self '%curtoken)
		   (concatenate 'string
				(slot-value self '%curtoken)
				(list c)))
	     (1+ j))
	   (progn
	     (%add-token self (slot-value self '%curtoken))
	     (setf (slot-value self '%parse1) #'%parse-main)
	     (1+ j))))
      (t
       (%add-token self (slot-value self '%curtoken))
       (setf (slot-value self '%parse1) #'%parse-main)
       (1+ j)))))
       
(defmethod %parse-string-1 ((self ps-base-parser) s i)
  (let ((c (string (aref s i))))
    (cond
      ((and
	(ppcre:scan +oct-string+ c)
	(< (length (slot-value self 'oct)) 3))
       (str-append (slot-value self 'oct) c)
       (1+ i))
      ((not (equal (slot-value self 'oct) ""))
       (str-append
	(slot-value self '%curtoken)
	(list
	 (code-char (parse-integer (slot-value self 'oct)
				  :radix 8))))
       (setf (slot-value self '%parse1) #'%parse-string)
       i)
      (t
       (when (assoc (char c 0) +esc-string+ :test 'char=)
	 ;(print c)
	 (str-append
	  (slot-value self '%curtoken)
	  (list (code-char (cdr (assoc (char c 0) +esc-string+ :test #'char=))))))
       (setf (slot-value self '%parse1) #'%parse-string)
       (1+ i)))))

(defmethod %parse-wopen ((self ps-base-parser) s i)
  (let ((c (char s i)))
    (cond
      ((char= c #\<)
       (%add-token self +keyword-dict-begin+)
       (setf (slot-value self '%parse1) #'%parse-main)
       (1+ i))
      (t
       (setf (slot-value self '%parse1) #'%parse-hexstring)
       i))))

(defmethod %parse-wclose ((self ps-base-parser) s i)
  (let ((c (char s i)))
    (setf (slot-value self '%parse1) #'%parse-main)
    (cond
      ((char= c #\>)
       (%add-token self +keyword-dict-end+)
       (1+ i))
      (t
       i))))

(defmethod %parse-hexstring ((self ps-base-parser) s i)
  (let* ((j (ppcre:scan +end-hex-string+ s :start i)))
    (setf (slot-value self '%curtoken)
	  (concatenate 'string
		       (slot-value self '%curtoken)
		       (subseq s i j)))
    (if (not j)
	(length s)
	(let ((token
	       (ppcre:regex-replace-all
		+HEX-PAIR+
		(ppcre:regex-replace-all
		 +SPC+
		 (slot-value self '%curtoken)
		 "")
		(lambda (ts s e ms me rs re)
		  (declare (ignore s e rs re))
		  (string
		   (code-char
		    (parse-integer
		     (subseq ts ms me)
		     :radix 16)))))))
	  (%add-token self token)
	  (setf (slot-value self '%parse1) #'%parse-main)
	  j))))
					
(defmethod parser-nexttoken ((self ps-base-parser))
  (loop
     while (not (slot-value self '%tokens))
       do (parser-fillbuf self)
       (setf (slot-value self 'charpos)
	     (funcall (slot-value self '%parse1)
		      self
		      (slot-value self 'buf)
		      (slot-value self 'charpos))))
  (pop (slot-value self '%tokens)))
 
(defclass ps-stack-parser (ps-base-parser)
  (context
   curtype
   curstack
   results)
  (:documentation "no docs"))

(defmethod initialize-instance :after ((self ps-stack-parser) &rest r)
  (declare (ignore r))
	   (parser-reset self))

(defmethod parser-reset ((self ps-stack-parser))
  (with-slots (context curtype curstack results) self
    (setf context nil
	  curtype nil
	  curstack nil
	  results nil)))

(defmethod parser-seek ((self ps-stack-parser) pos)
  (call-next-method)
  (parser-reset self))

(defmethod parser-push ((self ps-stack-parser) &rest objs)
  (with-slots (curstack) self
    (loop for item in objs
	 do (push item curstack))))

(defmethod parser-pop ((self ps-stack-parser) n)
  (with-slots (curstack) self
    (if (> n (length curstack))
	(prog1
	    (nreverse curstack)
	  (setf curstack nil))
	(let*
	    ((split (nthcdr (1- n) curstack))
	     (objs curstack))
	  (setf curstack (cdr split))
	  (setf (cdr split) nil)
	  (nreverse objs)))))
    
(defmethod parser-popall ((self ps-stack-parser))
  (with-slots (curstack) self
    (prog1
	(nreverse curstack)
      (setf curstack nil))))

(defmethod parser-add-results ((self ps-stack-parser) &rest objs)
  (with-slots (results) self
    (loop for item in objs
	 do (push item results))))

(defmethod parser-start-type ((self ps-stack-parser) pos type)
  (with-slots (context curtype curstack) self
    (push (list pos curtype curstack) context)
    (setf curtype type
	  curstack nil)))

(defmethod parser-end-type ((self ps-stack-parser) type)
  (with-slots (curtype curstack context) self
    ;(format *error-output* "context: ~s~%" context)
      (when (not (eql curtype type))
	(error "Type mismatch: ~s != ~s" curtype type))
    (let
	((objs
	  (nreverse
	  (loop for (_ . obj) in curstack
	       collect obj))))
      (destructuring-bind (pos oldtype oldstack)
	  (pop context)
    ;(format *error-output* "context: ~s~%" context)
	(setf curtype oldtype
	      curstack oldstack)
	(cons pos objs)))))

(defmethod parser-do-keyword ((self ps-stack-parser) pos token))

(defmethod parser-nextobject ((self ps-stack-parser))
  "Yields a list of objects.

Returns keywords, literals, strings, numbers, arrays and dictionaries.
Arrays and dictionaries are represented as Python lists and dictionaries."
  (with-slots (results context) self
  (loop while (not results)
       for (pos . token) = (parser-nexttoken self)
       do (cond
	    ((or
	      (typep token
		     '(or integer float boolean string))
	      (and (typep token 'symbol)
		   (eql (symbol-package token) *ps-literal-package*)))
	     (parser-push self (cons pos token)))
	    ((eql token +keyword-array-begin+)
	     (parser-start-type self pos :a))
	    ((eql token +keyword-array-end+)
	     (handler-case
		 (parser-push self (parser-end-type self :a))
	       (ps-type-error (v)
		 (when *strict* (error v)))))
	    ((eql token +keyword-dict-begin+)
		  (parser-start-type self pos :d))
	    ((eql token +keyword-dict-end+)
		 (handler-case
		     (progn
		       (destructuring-bind
			     (pos . objs) (parser-end-type self :d)
			 (when (not (evenp (length objs)))
			   (error "Invalid dictionary construct ~s" objs))
			 (parser-push
			  self
			  (cons pos
				(alexandria:plist-hash-table objs)
				#+or(alexandria:alist-hash-table
				 (loop for (k v . rest ) = objs then rest
				      while
				    k
				      when v
				      collect (cons k v)))))))
		   (ps-type-error (v)
		     (when *strict* (error v)))))
	    ((eql token +keyword-proc-begin+)
		 (parser-start-type self pos :p))
	    ((eql token +keyword-proc-end+)
	     (handler-case
		 (parser-push self (parser-end-type self :p))
	       (ps-type-error (var)
		 (when *strict* (error var)))))
	    (t
	     (parser-do-keyword self pos token)))
       (unless context
	 (parser-flush self)))
    (let ((obj (car (last results))))
      (setf results (butlast results))
      obj)))
	    
		     
		  
	
      
    

(defpackage test-ps-base-parser (:use :cl :pdfparse))
(in-package :test-ps-base-parser)
 
(defparameter +TESTDATA+ "%!PS
begin end
 \"  @ #
/a/BCD /Some_Name /foo#5f#xbaa
0 +1 -2 .5 1.234
(abc) () (abc ( def ) ghi)
(def\\040\\0\\0404ghi) (bach\\\\slask) (foo\\nbaa)
(this % is not a comment.)
(foo
baa)
(foo\\
baa)
<> <20> < 40 4020 >
<abcd00
12345>
func/a/b{(c)do*}def
[ 1 (z) ! ]
<< /foo (bar) >>
")

(defparameter +TOKENS+ 
  `(
      (5 . ,(KWD "begin")) (11 . ,(KWD "end")) (16 . ,(KWD "\"")) (19 . ,(KWD "@")) 
      (21 . ,(KWD "#")) (23 . ,(LIT "a")) (25 . ,(LIT "BCD")) (30 . ,(LIT "Some_Name")) 
      (41 . ,(LIT "foo_xbaa")) (54 . 0) (56 . 1) (59 . -2) (62 . 0.5d0) 
      (65 . 1.234d0) (71 . "abc") (77 . "") (80 . "abc ( def ) ghi") 
      (98 . "def   4ghi") (118 . "bach\\slask") (132 . "foo
baa") 
      (143 . "this % is not a comment.") (170 . "foo
baa") (180 . "foobaa") 
      (191 . "") (194 . " ") (199 . "@@ ") (211 . ,(coerce (mapcar #'code-char `(#xab #xcd #x00 #x12 ,(char-code #\4) #x05)) 'string)) 
      (226 . ,(KWD "func")) (230 . ,(LIT "a")) (232 . ,(LIT "b")) 
      (234 . ,(KWD "{")) (235 . "c") (238 . ,(KWD "do*")) (241 . ,(KWD "}")) 
      (242 . ,(KWD "def")) (246 . ,(KWD "[")) (248 . 1) (250 . "z") (254 . ,(KWD "!")) 
      (256 . ,(KWD "]")) (258 . ,(KWD "<<")) (261 . ,(LIT "foo")) (266 . "bar") 
      (272 . ,(KWD ">>"))
      ))

(defparameter +objs+
    `((23 . ,(lit "a")) (25 . ,(lit "BCD")) (30 . ,(lit "Some_Name"))
      (41 . ,(lit "foo_xbaa")) (54 . 0) (56 . 1) (59 . -2) (62 . 0.5d0)
      (65 . 1.234d0) (71 . "abc") (77 . "") (80 . "abc ( def ) ghi")
      (98 . "def   4ghi") (118 . "bach\\slask") (132 . "foo
baa")
      (143 . "this % is not a comment.") (170 . "foo
baa") (180 . "foobaa")
      (191 . "") (194 . " ") (199 . "@@ ") (211 . ,(coerce (mapcar #'code-char `(#xab #xcd #x00 #x12 ,(char-code #\4) #x05)) 'string))
      (230 . ,(lit "a")) (232 . ,(lit "b")) (234 . ("c")) (246 . (1 "z"))
      (258 . ,(alexandria:alist-hash-table
	      `((,(lit "foo")  "bar" ))))))

(defun get-tokens (s)
  (with-input-from-string (stream s)
    (let ((parser
	   (make-instance 'ps-base-parser :fp stream))
	  (r nil))
      (handler-case
       (loop
	    (push (parser-nexttoken parser) r))
       (ps-eof (v)(declare (ignore v)) (reverse r))))))

(defclass my-parser (ps-stack-parser) ())
(defmethod pdfparse::parser-flush ((self my-parser))
  (print 'flush)
  (apply #'pdfparse::parser-add-results self (pdfparse::parser-popall self)))

(defun get-objects (s)
  (let ((r))
  (handler-case
      (loop with parser = (make-instance 'my-parser :fp (make-string-input-stream s))
	   do (push (parser-nextobject parser) r))
    (ps-eof (v) (declare (ignore v)) (reverse r)))))

(defun test1 ()
  (assert (equal (get-tokens +testdata+)
		 +tokens+)))

(defun maxi-equal (obj1 obj2)
  (cond
    ((and (consp obj1) (consp obj2))
     (and (maxi-equal (car obj1) (car obj2))
	  (maxi-equal (cdr obj1) (cdr obj2))))
    ((and (hash-table-p obj1)
	  (hash-table-p obj2))
     (and
      (= (hash-table-count obj1)
	 (hash-table-count obj2))
      (loop for k being the hash-keys of obj1
	   unless
	   (and (nth-value 1 (gethash k obj2))
		(equal (gethash k obj1)
		       (gethash k obj1)))
	   return nil
	   finally (return t))))
    (t
     (if
      (equal obj1 obj2)
      t
      (prog1 nil (print obj1) (print obj2) (terpri))))))
     
(defun test2 ()
  (assert (maxi-equal (get-objects +testdata+) +objs+)))



;;pdftypes.py
(in-package #:pdfparse)

(defparameter +LITERAL-CRYPT+  (LIT "Crypt"))
; Abbreviation of Filter names in PDF 4.8.6. "Inline Images"
(defparameter +LITERALS-FLATE-DECODE+  (list (LIT "FlateDecode")  (LIT "Fl")))
(defparameter +LITERALS-LZW-DECODE+  (list (LIT "LZWDecode")  (LIT "LZW")))
(defparameter +LITERALS-ASCII85-DECODE+  (list (LIT "ASCII85Decode")  (LIT "A85")))
(defparameter +LITERALS-ASCIIHEX-DECODE+  (list (LIT "ASCIIHexDecode")  (LIT "AHx")))
(defparameter +LITERALS-RUNLENGTH-DECODE+  (list (LIT "RunLengthDecode")  (LIT "RL")))
(defparameter +LITERALS-CCITTFAX-DECODE+  (list (LIT "CCITTFaxDecode")  (LIT "CCF")))
(defparameter +LITERALS-DCT-DECODE+  (list (LIT "DCTDecode")  (LIT "DCT")))

(define-condition pdf-exception (error) ())
(define-condition pdf-type-error (pdf-exception) ())
(define-condition pdf-value-error (pdf-exception) ())
(define-condition pdf-not-implemented-error (pdf-exception) ())
(define-condition ps-syntax-error (error) ())

(defclass pdf-obj-ref ()
  ((objid :initarg :objid)
   (doc :initarg :doc)))


(defun make-pdf-obj-ref (doc objid _)
  (declare (ignorable _))
  (make-instance 'pdf-obj-ref :objid objid :doc doc))

(defmethod print-object ((self pdf-obj-ref) stream)
  (format stream "#<PDF-OBJ-REF ~D>" (slot-value self 'objid)))

(defmethod resolve ((self pdf-obj-ref))
  (with-slots (doc objid) self
  (getobj doc objid)))

(defun resolve1 (x)
  "Resolves an object.

If this is an array or dictionary, it may still contains
some indirect objects inside."
  (loop while (typep x 'pdf-obj-ref)
       do (setf x (resolve x)))
  x)


    
(defun resolve-all (x)
    "Recursively resolves the given object and all the internals.
    
Make sure there is no indirect reference within the nested object.
This procedure might be slow."
    (let ((x (resolve x)))
      (cond
	((listp x)
	 (mapcar #'resolve-all x))
	((hash-table-p x)
	 (alexandria:plist-hash-table
	  (loop for k being the hash-keys of x
	       using (hash-value v)
	       collect (cons k (resolve-all v)))
	  :test (hash-table-test x)))
	(t x))))

(defun decipher-all (decipher objid genno x)
  (typecase x
      (string (funcall decipher objid genno x))
      (list (mapcar (lambda (x) (decipher-all decipher objid genno x)) x))
      (hash-table
       (maphash (lambda (k v)
		  (setf (gethash k x) (decipher-all decipher objid genno v))) x)
       x)
      (t x)))

(defun int-value (x)
  (let ((x (resolve1 x)))
    (if (not (integerp x))
	(if *strict*
	    (error (make-condition 'pdf-type-error "Integer required: ~s" x))
	    0)
	x)))

(defun float-value (x)
  (let ((x (resolve1 x)))
    (if (not (floatp x))
	(if *strict*
	    (error (make-condition 'pdf-type-error "Float required: ~s" x))
	    0.0d0)
	x)))

(defun num-value (x)
  (let ((x (resolve1 x)))
    (if (not (numberp x))
	(if *strict*
	    (error (make-condition 'pdf-type-error "Number required: ~s" x))
	    0)
	x)))

(defun str-value (x)
  (let ((x (resolve1 x)))
    (if (not (stringp x))
	(if *strict*
	    (error (make-condition 'pdf-type-error "String required: ~s" x))
	    "")
	x)))

(defun list-value (x)
  (let ((x (resolve1 x)))
    (if (not (listp x))
	(if *strict*
	    (error (make-condition 'pdf-type-error "List required: ~s" x))
	    nil)
	x)))

(defun dict-value (x)
  (let ((x1 (resolve1 x)))
    (if (not (hash-table-p x1))
	(if *strict*
	    (error (make-condition 'pdf-type-error "Hash-Table required: ~s" x1))
	    (make-hash-table))
	x1)))

(defun stream-value (x)
  (let ((x (resolve1 x)))
    (if (not (typep x 'pdf-stream))
	(if *strict*
	    (error (make-condition 'pdf-type-error "Integer required: ~s" x))
	    (make-pdf-stream (make-hash-table) ""))
	x)))

(defclass pdf-stream ()
  (attrs
   rawdata
   decipher
   data
   objid
   genno))

(defun make-pdf-stream (attrs rawdata &optional decipher)
  (assert (hash-table-p attrs))
  (let ((self (make-instance 'pdf-stream)))
    (setf
     (slot-value self 'attrs) attrs
     (slot-value self 'rawdata) rawdata
     (slot-value self 'decipher) decipher
     (slot-value self 'data) nil
     (slot-value self 'objid) nil
     (slot-value self 'genno) nil)
    self))

(defmethod set-objid ((self pdf-stream) %objid %genno)
  (with-slots (objid genno) self
      (setf objid %objid
	    genno %genno)))

(defmethod print-object ((self pdf-stream) stream)
  (with-slots (data rawdata objid attrs) self
    (if (not data)
	(progn
	  (assert rawdata)
	(format stream "#<PDF-STREAM ~s raw=~d ~s>"
		objid (length rawdata) (alexandria:hash-table-alist attrs)))
	(progn
	  (assert data)
	  (format stream "#<PDF-STREAM ~s len=~d ~s>"
		  objid (length data) (alexandria:hash-table-alist attrs))))))

(defmethod contains ((self pdf-stream) name)
  (nth-value 1 (gethash name (slot-value self 'attrs))))

(defmethod getitem ((self pdf-stream) name &optional default)
  (gethash name (slot-value self 'attrs) default))

(defmethod getitem ((self hash-table) name &optional default)
  (gethash name self default))

(defmethod get-any ((self pdf-stream) names &optional default)
  (loop for name in names
       for (val present) = (multiple-value-list
			     (gethash name (slot-value self 'attrs)))
       when present return val
       finally (return default)))

(defmethod get-filters ((self pdf-stream))
  (let ((filters
	 (get-any self (list (lit "F") (lit "Filter")))))
    (if (listp filters) filters (list filters))))

(defmethod decode ((self pdf-stream))
  (with-slots (rawdata decipher objid genno) self
  (assert (and
	   (null (slot-value self 'data)) 
	   rawdata))
  (let* ((data rawdata)
	 (data
	  (if decipher
	      (funcall decipher objid genno data)
	      data))
	 (filters (get-filters self)))
    (cond
      ((null filters)
       (setf (slot-value self 'data) data
	     rawdata nil))
      (t
       (loop for f in filters
	    do
	    (cond
	      ((member f +literals-flate-decode+)
	       (setf data
		     (flexi-streams:with-output-to-sequence
			 (outstream :element-type 'character
				    :transformer #'code-char)
		       (deflate:inflate-zlib-stream
			(8bitstring-to-stream data)
			outstream
			:check-checksum t))))
	     (t (error (make-condition 'pdf-not-implemented-error "Unsupported filter ~S" f))))
	    (let ((params (get-any self
				   (mapcar #'litf
					   '("DP" "DecodeParms" "FDecodeParms")) (make-hash-table))))
	      (when
		  (and
		   (in-dict (lit "Predictor") params)
		   (in-dict (lit "Columns") params))
		(let ((pred (int-value (gethash (lit "Predictor") params)))
		      (columns (int-value (gethash (lit "Columns") params))))
		  (when (and pred (/= pred 0))
		    (when (/= pred 12)
		      (error (make-condition 'pdf-not-implemented-error
					     "Unsupported predictor ~s" pred)))
		    (setf data (png-predictor data columns)))))))
       (setf (slot-value self 'data) data
	     rawdata nil))))))

(defun png-predictor (data columns)
  (with-output-to-string (buf)
    (let ((ent0 (make-string columns :initial-element (code-char 0))))
      (loop for i from 0 below (length data) by (1+ columns)
	 do (let ((pred (aref data i))
		  (ent1 (subseq data (1+ i) (+ i 1 columns))))
	      (when (= (char-code pred) 2)
		(setf
		 ent1
		 (concatenate
		  'string
		  (loop for a across ent0
		     for b across ent1
		     collect
		       (code-char (mod (+ (char-code a) (char-code b)) 256))))))
	      (write-string ent1 buf)
	      (setf ent0 ent1))))))

(defmethod get-data ((self pdf-stream))
  (with-slots (data) self
    (unless data (decode self))
    data))


;# some predefined literals and keywords.
(defparameter +LITERAL-OBJSTM+ (LIT "ObjStm"))
(defparameter +LITERAL-XREF+ (LIT "XRef"))
(defparameter +LITERAL-PAGE+ (LIT "Page"))
(defparameter +LITERAL-PAGES+ (LIT "Pages"))
(defparameter +LITERAL-CATALOG+ (LIT "Catalog"))

(define-condition pdf-no-valid-xref (error) ())

(defclass pdf-base-xref () ())

(defmethod get-trailer ((self pdf-base-xref))
  (error (make-condition 'pdf-not-implemented-error)))

(defmethod get-objids ((self pdf-base-xref))
  nil)

(defmethod get-pos ((self pdf-base-xref) objid)
  (error (make-condition 'key-error objid)))

(defclass pdf-xref (pdf-base-xref)
  (offsets
   trailer))

(defun make-pdf-xref ()
  (let ((self (make-instance 'pdf-xref)))
    (with-slots (offsets trailer) self
	(setf offsets (make-hash-table)
	      trailer (make-hash-table)))
    self))

(defmethod pdf-load ((self pdf-xref) parser &optional (debug 1))
  (let ((pos) (line))
    (block break
       (loop
	  (tagbody continue
	     (handler-case
		 (progn
		   (let ((v (parser-nextline parser)))
		     (setf pos (car v)
			   line (cdr v)))
		   (unless line
		     (error (make-condition 'pdf-no-valid-xref "Premature eof: ~s" parser)))
		   (when (string= (strip line) "")
		     (go continue)))
	       (ps-eof () (error (make-condition 'pdf-no-valid-xref "Unexpected EOF - file corrupted?"))))
	     (when (starts-with-subseq "trailer" line)
	       (parser-seek parser pos)
	       (return-from break))
	     (let ((f (split-sequence #\Space (strip line))))
	       (when (/= (length f) 2)
		 (error (make-condition 'pdf-no-valid-xref "Trailer not fount: ~s: line=~s" parser line)))
	       (destructuring-bind
		     (start nobjs) (mapcar #'parse-integer f)
		 (loop for objid from start below (+ start nobjs)
		    for (_ . line) = (parser-nextline parser)
		    for f = (split-sequence #\Space (strip line))
		    for (pos genno use . r) = f
		    when (/= (length f) 3)
		    do (error (make-condition 'pdf-no-valid-xref "Invalid XRef format: ~s, line=~s" parser line))
		    when (string= use "n")
		    do (setf
			(gethash objid (slot-value self 'offsets))
			(cons (parse-integer genno)
			      (parse-integer pos))))))))))
  (when (<= 1 debug)
    (format *error-output* "xref objects: ~s~%" (slot-value self 'offsets)))
  (load-trailer self parser)
  )

(defparameter +keyword-trailer+ (kwd "trailer"))

(defmethod load-trailer ((self pdf-xref) parser)
  (let ((kwd) (dic))
    (handler-case
	(progn
	  (setf kwd (cdr (parser-nexttoken parser))
		dic (cdr (parser-nextobject parser)))
	  (format *error-output* "Trailer: ~s~%" (hash-table-alist dic))
	  (assert (eql kwd +keyword-trailer+)))
      (ps-eof ()
	  (let ((x (parser-pop parser 1)))
	    (unless x
	      (error (make-condition 'pdf-no-valid-xref "Unexpected EOF - file corrupted")))
	    (setf dic (car x)))))
    (let ((dic (dict-value dic)))
	(format *error-output* "Trailer: ~s~%" (hash-table-alist dic))
      (loop
	 for k being the hash-key of dic
	 using (hash-value v)
	   do (setf (gethash k (slot-value self 'trailer)) V)))))

(defparameter +pdfobj-cue+ 
		  (ppcre:create-scanner "^(\\d+)\\s+(\\d+)\\s+obj\\b"))

(defmethod load-fallback ((self pdf-xref) parser &optional (debug 0))
  (parser-seek parser 0)
  (loop
       for (pos . line) =
       (handler-case
	   (parser-nextline parser)
	 (ps-eof () (return)))
     when (starts-with-subseq "trailer" line)
     do (parser-seek parser pos)
       (load-trailer self parser)
       (when (<= 1 debug)
	 (format *error-output* "trailer: ~s~%" (get-trailer self)))
       (return)
     do (multiple-value-bind
	      (ms me rs re)
	      (ppcre:scan +pdfobj-cue+ line)
	    (declare (ignore ms me))
	  (when rs
	    (let ((objid (subseq line (car rs) (car re))))
	      (setf (gethash (parse-integer objid)
			     (slot-value self 'offsets))
		    (cons 0 pos)))))))
		  
(defmethod get-trailer ((self pdf-xref))
  (slot-value self 'trailer))

(defmethod get-objids ((self pdf-xref))
  (loop for k being the hash-keys of (slot-value self 'offsets)
       collect k))

(defmethod get-pos ((self pdf-xref) objid)
  (multiple-value-bind
	(v present)
      (gethash objid (slot-value self 'offsets))
    (when (not present)
      (error (make-condition 'key-error objid)))
    (cons nil (cdr v))))

(defclass pdf-xref-stream (pdf-base-xref)
  ((data :initform nil)
   (entlen :initform nil)
   (fl1 :initform nil)
   (fl2 :initform nil)
   (fl3 :initform nil)
   (objid-ranges :initform nil)))

(defun make-pdf-xref-stream ()
  (make-instance 'pdf-xref-stream))

(defmethod print-object ((self pdf-xref-stream) stream)
  (with-slots (fl1 fl2 fl3) self
    (format stream "#<PDF-XREF-STREAM ~d ~d ~d>"
	    fl1 fl2 fl3)))

(defun get-start-id (range)
  (car range))

(defun get-end-id (range)
  (1- (+ (car range) (cdr range))))

(defun get-nobjs (range)
  (cdr range))

(defmethod pdf-load ((self pdf-xref-stream) parser &optional (debug 0))
  (with-slots (objid-ranges fl1 fl2 fl3 data trailer)
  (parser-nexttoken parser) ;obj id
  (parser-nexttoken parser) ;genno
  (parser-nexttoken parser) ;kwd
  (let* ((stream (cdr (parser-nextobject parser))))
    (unless (typep stream 'pdf-stream)
      (error (make-condition 'pdf-no-valid-xref "Invalid PDF stream spec.")))
    (let* ((size (getitem stream (lit "Size")))
	   (index-array (getitem stream (lit "Index") (cons 0 size))))
      (unless (evenp (length index-array))
	(error (make-condition 'pdf-no-valid-xref "Invalid index number")))
      (setf objid-ranges
	    (nconc
	     objid-ranges
	     (loop
		  for (start nobjs . rest) = index-array then rest
		  collect (cons start nobjs)
		  while rest)))
      (let ((w (getitem stream (lit "W"))))
	(setf
	 fl1 (first w)
	 fl2 (second w)
	 fl3 (third w)
	 data (get-data stream)
	 trailer (slot-value stream 'attrs)))))
      (when
	  (<= 1 debug)
	(format *error-output* "xref stream: obj-id=~{~a~^, ~} fields=~d,~d,~d~%"
		objid-ranges fl1 fl2 fl3))))

(defmethod get-trailer ((self pdf-xref-stream))
  (slot-value self 'trailer))

;NB This was a generator; since it doesn't do anything but calculate a (possible large) list of numbers, it should be fairly fast.  I'll just do it once.
(defmethod get-objids ((self pdf-xref-stream))
  (loop
     for range in (slot-value self 'objid-ranges)
     append
       (loop for i from (get-start-id range) to (get-end-id range)
	  collect i)))

(defun nunpack (s &optional (default 0))
  (assert (<= (length s) 4))
  (if (= (length s) 0)
      default
  (let ((val 0))
    (loop for c across s
       do (setf val (+ (* val 256) (char-code c))))
    val)))

(defmethod get-pos ((self pdf-xref-stream) objid)
  (with-slots (objid-ranges entlen fl1 fl2 fl3 data) self
    (let ((offset 0)
	  (found nil))
      (loop for range in objid-ranges
	   when (and
		 (>= objid (get-start-id range))
		 (<= objid (get-end-id range)))
	 do (incf offset (- objid (get-start-id range)))
	   (setf found t)
	   (return)
	 do (incf offset (get-nobjs range)))
      (when (not found)
	(error (make-condition 'key-error objid)))
      (let* ((i (* offset entlen))
	     (ent (subseq data i (+ i entlen)))
	     (f1 (nunpack (subseq ent 9 fl1) 1)))
	(cond
	  ((= f1 1)
	   (cons nil (nunpack (subseq ent fl1 (+ fl1 fl2)))))
	  ((= f1 2)
	   (cons
	    (nunpack (subseq ent fl1 (+ fl1 fl2)))
	    (nunpack (subseq ent (+ fl1 fl2)))))
	  (t
	   (error (make-condition 'key-error objid))))))))


(defclass pdf-page ()
  (doc
   pageid
   attrs
   lastmod
   resources
   mediabox
   cropbox
   rotate
   annots
   beads
   contents)
  (:documentation
    "An object that holds the information about a page.

A PDFPage object is merely a convenience class that has a set
of keys and values, which describe the properties of a page
and point to its contents.

Attributes:
  doc: a PDFDocument object.
  pageid: any Python object that can uniquely identify the page.
  attrs: a dictionary of page attributes.
  contents: a list of PDFStream objects that represents the page content.
  lastmod: the last modified time of the page.
  resources: a list of resources used by the page.
  mediabox: the physical size of the page.
  cropbox: the crop rectangle of the page.
  rotate: the page rotation (in degree).
  annots: the page annotations.
  beads: a chain that represents natural reading order.
"))

(defun make-pdf-page (%doc %pageid %attrs)
  (let ((self (make-instance 'pdf-page)))
    (with-slots (doc pageid attrs lastmod resources mediabox cropbox rotate
		     annots beads contents)
	self
      (setf
       doc %doc
       pageid %pageid
       attrs (dict-value %attrs)
       lastmod (resolve1 (gethash (lit "LastModified") attrs))
       resources (resolve1 (gethash (lit "Resources") attrs))
       mediabox (resolve1 (gethash (lit "MediaBox") attrs))
       cropbox (if (in-dict (lit "CropBox") attrs)
		   (resolve1 (gethash (lit "CropBox") attrs))
		   mediabox)
       rotate (mod (+ 360 (getitem attrs (lit "Rotate") 0)) 360)
       annots (getitem attrs (lit "Annots"))
       beads (getitem attrs (lit "B"))
       contents (when (in-dict  (lit "Contents") attrs)
		  (resolve1 (gethash (lit "Contents") attrs)))
       contents (if (listp contents)
		    contents
		    (list contents))))
    self))
       
(defclass pdf-document ()
  (caching
   (info :initform nil)
   (catalog :initform nil)
   (encryption :initform nil)
   (decipher :initform nil)
   (xrefs :initform nil)
   (%parser :initform nil)
   (%cached-objs :initform (make-hash-table))
   (%parsed-objs :initform (make-hash-table))
   is-printable is-modifiable is-extractable)
    (:documentation "PDFDocument object represents a PDF document.

    Since a PDF file can be very big, normally it is not loaded at
    once. So PDF document has to cooperate with a PDF parser in order to
    dynamically import the data as processing goes.

    Typical usage:
      doc = PDFDocument()
      doc.set_parser(parser)
      doc.initialize(password)
      obj = doc.getobj(objid)
    
    "))

(defun make-pdf-document (&key (caching t))
  (let ((self (make-instance 'pdf-document)))
    (setf (slot-value self 'caching) caching)
    self))

(defmethod (setf parser) (parser (self pdf-document))
  (with-slots (%parser xrefs encryption info catalog) self
    (unless %parser
      (setf %parser parser
	    xrefs (parser-read-xref parser))
      (loop for xref in xrefs
	 for trailer = (get-trailer xref)
	 when trailer
	 do
	   (when (in-dict (lit "Encrypt") trailer)
	     (setf encryption
		   (cons
		    (list-value (gethash (lit "ID") trailer))
		    (dict-value (gethash (lit "Encrypt") trailer)))))
	   (when (in-dict (lit "Info") trailer)
	     ;BUG?? does order matter?
	     (push (dict-value (gethash (lit "Info") trailer)) info))
	   (when (in-dict (lit "Root") trailer)
	     (setf catalog
		   (dict-value (gethash (lit "Root") trailer)))
	     (return))
	 finally (error "No /Root object! - Is this really a PDF?"))
      (when (and *strict* (not (eql (gethash (lit "Type") catalog)
				    +literal-catalog+)))
	(error "Catalog not found")))))

(defun update-digest (digester string &rest r)
  (apply
   #'ironclad:update-digest
   digester
   (make-array (length string) :element-type '(unsigned-byte 8)
	       :initial-contents
	       (map 'list #'char-code string))
   r))

(defun make-le-int32 (v)
  (if (>= v 0)
      (make-le-uint32 v)
      (make-le-uint32 (+ v (expt 2 32)))))

(defun make-le-uint32 (v)
  (let ((ar (make-array 4 :element-type '(unsigned-byte 8))))
    (loop for i from 0 below 4
	 do (setf (aref ar i) (logand v #xff)
	       v (ash v -8)))
    ar))

(defparameter +password-padding+
  (map 'string #'code-char
'(40 191 78 94 78 117 138 65 100 0 78 86 255 250 1 8 46 46 0 182 208
104 62 128 47 12 169 254 100 83 105 122)))

(defun arcfour-process (key text)
  (let* ((arcfour (ironclad:make-cipher 'ironclad:arcfour :key key :mode :stream))
	 (plaintext (make-array
		     (length text)
		     :element-type '(unsigned-byte 8))))
    (ironclad:decrypt arcfour text plaintext)
    plaintext))
  

(defun getauthtoken (r key docid)
  (let ((arcfour (ironclad:make-cipher 'ironclad:arcfour :key key :mode :stream)))
    (if (= r 2)
	(let ((plaintext (make-array
			  (length +password-padding+)
			  :element-type '(unsigned-byte 8))))
	  (ironclad:decrypt arcfour
			    (map '(simple-array (unsigned-byte 8)) #'char-code
				 +password-padding+)
			    plaintext)
	  (map 'string #'code-char plaintext))
	(let ((hash (ironclad:make-digest 'ironclad:md5)))
	  (update-digest hash +password-padding+)
	  (update-digest hash (elt docid 0))
	  (let ((x (arcfour-process key
				    (subseq (ironclad:produce-digest hash) 0 16))))
	    (loop for i from 1 to 19
	       for k = (make-array (length key)
				   :element-type '(unsigned-byte 8)
				   :initial-contents (loop for c across key
							collect (logxor c i)))
	       do (setf x (arcfour-process k x)))
	    (map 'string #'code-char x))))))
	   

(defun decrypt-rc4 (key objid genno data)
  (let* ((key1 (concatenate '(vector (unsigned-byte 8))
				key
				(subseq (make-le-uint32 objid) 0 3)
				(subseq (make-le-uint32 genno) 0 2)))
	 (hash (ironclad:make-digest 'ironclad:md5))
	 (data-bin
	  (map-into (make-array (length data) :element-type '(unsigned-byte 8))
		    #'char-code data)))
    (ironclad:update-digest hash key1)
    (map-into (make-string (length data)) #'code-char
    (arcfour-process (subseq (ironclad:produce-digest hash)
			     0 (min (length key1) 16)) 
		     data-bin))))
    
	 
    

(defmethod initialize ((self pdf-document) &optional (password ""))
  (declare (ignorable password))
  (with-slots (is-printable is-modifiable is-extractable encryption decrypt-key decipher) self
    (if (or (not encryption)
	    (= 0 (hash-table-count (cdr encryption))))
	(setf is-printable t
	      is-modifiable t
	      is-extractable t)
	(destructuring-bind (docid . param) encryption
	  (unless (eql (lit "Standard") (gethash (lit "Filter") param))
	    (error "Unknown filter: param=~s" param))
	  (let*
	      ((v (int-value (gethash (lit "V") param 0)))
	       (length (int-value (gethash (lit "Length") param 40)))
	       (o (str-value (gethash (lit "O") param)))
	       (r (int-value (gethash (lit "R") param)))
	       (u (str-value (gethash (lit "U") param)))
	       (p (int-value (gethash (lit "P") param))))
	    (unless (or (= v 1) (= v 2))
	      (error "Unknown algorithm: param=~s" param))
	    (when (<= 5 R)
	      (error "Unknown revision: ~s" r))
	    (setf is-printable (/= 0 (logand p 4))
		  is-modifiable (/= 0 (logand p 8))
		  is-extractable (/= 0 (logand p 16)))
	    (let* ((password (concatenate 'string password +password-padding+))
		   (hash (ironclad:make-digest 'ironclad:md5))
		   (key))
	      (update-digest hash password)
	      (update-digest hash o)
	      (update-digest hash (map 'string #'code-char (make-le-int32 p)))
	      (update-digest hash (elt docid 0))
	      (when (<= 4 R)
		(error "Revision 4 encryption is currently unsupported"))
	      (when (<= 3 R)
		(loop for i from 1 to 50
		     for digest = (subseq (ironclad:produce-digest hash)
					  0 (/ length 8))
		     do (setf hash (ironclad:make-digest 'ironclad:md5))
		     (ironclad:update-digest hash digest)))
	      (setf key (subseq (ironclad:produce-digest hash)
				0 (/ length 8)))
	      (let* ((u1 (getauthtoken r key docid)))
		#+(or)(format *error-output* "~S ~S~%~S~%" r (map 'list #'char-code u1)
			(map 'list #'char-code u))
		(unless
		    (if (= r 2)
			(string= u1 u)
			(string= (subseq u1 0 16) (subseq u 0 16)))
		  (error "PDF Password Incorrect")))
	      ;(setf decrypt-key key)
	      (setf decipher 
		    (lambda (id genno data)
		      (decrypt-rc4 key id genno data)))))))))
	      
	    
(defparameter +keyword-obj+ (kwd "obj"))
(defmethod getobj ((self pdf-document) objid)
  (with-slots (xrefs caching %cached-objs %parsed-objs %parser decipher) self
  (when (not xrefs) (error "PDF-DOCUMENT is not initialized"))
  (when (<= 2 *pdf-document-debug*)
    (format *error-output* "getobj: objid=~s~%" objid))
  (multiple-value-bind (obj genno)
      (if (in-dict objid %cached-objs)
	  (values (gethash objid %cached-objs) 0)
	  (destructuring-bind
		     (strmid . index)
	      (loop for xref in xrefs
		 for pair = (get-pos xref objid)
		 when pair return it
		 finally (if *strict*
			     (error "Cannot locate objid=~s" objid)
			     (return-from getobj nil)))
	    (if strmid
		(let* ((stream (stream-value (getobj self strmid)))
		       (n (getitem stream (lit "N")))
		       (n (if n n (if *strict*
				      (error "Not a stream object: ~s" stream)
				      0)))
		       (objs (if (in-dict strmid %parsed-objs)
				 (gethash strmid %parsed-objs)
				 (let
				     ((parser (make-pdf-stream-parser (get-data stream))))
				   (setf (parser-document parser) self)
				   (setf
				    (gethash strmid %parsed-objs)
				    (handler-case
					(loop
					   for pair =
					     (handler-case
						 (parser-nextobject parser)
					       (ps-eof () :eof))
					     until (eql pair :eof)
					   collect (cdr pair)))))))
		       (obj (nth (+ index (* n 2)) objs)))
		  (when (not obj) (error "Invalid object number: objid=~s" objid))
		  (when (typep obj 'pdf-stream)
		    (set-objid obj objid 0))
		  (values obj 0))
		(let*
		    ((_ (parser-seek %parser index))
		     (objid1 (cdr (parser-nexttoken %parser)))
		     (genno (cdr (parser-nexttoken %parser)))
		     (kwd (cdr (parser-nexttoken %parser))))
		  (declare (ignore _))
		  ;(format *error-output* "FOO: ~S ~S ~S~%" objid1 genno kwd)
		  (when (not (eql  objid1 objid))
		    (let ((x (loop
				  with x = nil
				while (not (eql kwd +keyword-obj+))
				do
				  (setf kwd (cdr (parser-nexttoken %parser)))
				  (push kwd x)
				  finally (return x))))
		      (when x
			(setf objid1 (second x)
			      genno (first x)))))
		  (unless (eql kwd +keyword-obj+)
		    (error "Invalid object spec: offset=~s" index))
		  (handler-case
		      (let
			  ((obj (cdr (parser-nextobject %parser))))
			(when (typep obj 'pdf-stream)
			  (set-objid obj objid genno))
			(values obj genno))
		    (ps-eof () (return-from getobj nil)))))))
    (when (not (in-dict objid %cached-objs))
      (when (<= 2 *pdf-document-debug*)
	(format *error-output* "register: objid=~s: ~s" objid obj))
      (when caching
	(setf (gethash objid %cached-objs) obj)))
    (if decipher
	(decipher-all decipher objid genno obj)
	obj))))
      
(defparameter +inheritable-attrs+ (list (lit "Resources") (lit "MediaBox")
					(lit "CropBox")(lit "Rotate")))

;NB Hairy function, probably has bugs
(defmethod get-pages ((self pdf-document))
  (with-slots (xrefs catalog) self
    (when (not xrefs)
      (error "PDF-DOCUMENT is not initialzied"))
    (labels
	 ((search-tree (obj parent)
	    (let ((nested-lambda nil)
		  (kids nil))
	      (multiple-value-bind
		    (objid tree)
		  (if (typep obj 'integer)
		      (values obj (copy-hash-table (dict-value (getobj self obj))))
		      (values (slot-value obj 'objid)
			      (copy-hash-table (dict-value (getobj self (slot-value obj 'objid))))))
		(loop for k being the hash-keys of parent
		     using (hash-value v)
		     when (and
			   (member k +inheritable-attrs+)
			   (not (in-dict k tree)))
		     do (setf (gethash k tree) v))
		(cond
		  ((and
		    (eql (gethash (lit "Type") tree) +literal-pages+)
		    (in-dict (lit "Kids") tree))
		   (when (<= 1 *pdf-document-debug*)
		     (format *error-output* "Pages: Kids=~s~%" (gethash (lit "Kids") tree)))
		   (setf kids
			 (list-value (gethash (lit "Kids") tree)))
		   (let ((me (pop kids)))
		     (setf nested-lambda
			   (search-tree me tree)))
		   (lambda ()
		     (let ((x (funcall nested-lambda)))
		       (if (eql x :end)
			   (if (null kids) :end
			       (loop for c = (pop kids)
				  for iterator = (search-tree c tree)
				  when (eql iterator :end) return :end
				  do (let ((v (funcall iterator)))
				       (unless (eql v :end)
					 (setf nested-lambda iterator)
					 (return v)))))
			   x))))
		    ((eql (gethash (lit "Type") tree) +literal-page+)
		     (setf kids t)
		     (lambda ()
		       (if kids
			   (progn
			     (setf kids nil)
			     (make-pdf-page self objid tree))
			    :end)))
		    (t (lambda () :end)))))))
      (if (in-dict (lit "Pages") catalog)
	  (search-tree (gethash (lit "Pages") catalog) catalog)
	  (lambda () :end)))))
		 
;NB non-trivial geneator code, may be buggy
(defmethod get-outlines ((self pdf-document))
  (with-slots (catalog) self
  (when (not (in-dict "Outlines"catalog))
    (error "No Outlines"))
  (labels
      ((outline-search (entry level)
	 (let
	     ((entry (dict-value entry))
	      (internal-lambda nil)
	      (state :before-title))
	   (lambda ()
	     (block yield
	       (tagbody
		  (case state
		    (:before-title (go before-title))
		    (:before-first (go before-first))
		    (:in-first (go in-first))
		    (:in-next (go in-next))
		    (:done (return-from yield :end)))
		before-title
		  (when (and (in-dict (lit "Title") entry)
			     (or (in-dict  "A" entry)
				 (in-dict "Dest" entry)))
		    (setf state :before-first)
		    (return-from yield
		      (list 
		       (decode-text (str-value (gethash (lit "Title") entry)))
		       (gethash (lit "Dest") entry)
		       (gethash (lit "A") entry)
		       (gethash (lit "SE") entry))))
		before-first 
		  (if (and (in-dict (lit "First") entry)
			   (in-dict (lit "Last") entry))
		      (setf internal-lambda
			    (outline-search (gethash (lit "First") entry) (1+ level)))
		      (go before-next))
		in-first
		  (setf state :in-first)
		  (let ((v (funcall internal-lambda)))
		    (unless (eql v :end) (return-from yield v)))
		before-next
		  (if (in-dict (lit "Next") entry)
		      (setf internal-lambda
			    (outline-search (gethash (lit "Next") entry) level))
		      (return-from yield (setf state :done)))
		in-next
		  (setf state :in-next)
		  (let ((v (funcall internal-lambda)))
		    (if
		     (eql v :end)
		     (return-from yield (setf state :done))
		     (return-from yield v)))))))))
    (outline-search (gethash (lit "Outlines") catalog) 0))))

(defmethod lookup-name ((self pdf-document) cat key)
  (with-slots (catalog) self
    (let*
      ((names (handler-case
		  (dict-value (gethash (lit "Names") catalog))
		(t () (error (make-condition 'key-error cat key)))))
       (d0 (dict-value (gethash cat names))))
      (labels
	  ((lookup (d)
	     (when (in-dict (lit "Limits") d)
		 (destructuring-bind (k1 k2) (list-value (gethash (lit "Limits") d))
		   (when (or (< key k1) (< k2 key))
		     (return-from lookup nil))
		   (when (in-dict (lit "Names") d)
		     (return-from lookup
		       (gethash key
				(plist-hash-table (list-value (gethash (lit "Kids") d))))))))
	     (when (in-dict (lit "Kids") d)
	       (loop for c in (list-value (gethash (lit "Kids") d))
		    for v = (lookup (dict-value c))
		    when v do (return-from lookup v)))
	     (error (make-condition 'key-error cat key))))
	(lookup d0)))))

(defmethod get-dest ((self pdf-document) name)
  (handler-case
      (lookup-name self (lit "Dests") name)
    (key-error ()
      (with-slots (catalog) self
	(unless (in-dict (lit "Dests") catalog)
	  (error "Destination not found ~s" name))
	(let ((d0 (dict-value (gethash "Dests" catalog))))
	  (unless (in-dict name d0)
	    (error "Destination not found ~s" name))
	  (gethash name d0))))))


(defclass pdf-parser (ps-stack-parser)
  ((doc :initform nil)
   (fallback :initform nil))
  (:documentation
"PDFParser fetch PDF objects from a file stream.
It can handle indirect references by referring to
a PDF document set by set_document method.
It also reads XRefs at the end of every PDF file.

Typical usage:
  parser = PDFParser(fp)
  parser.read_xref()
  parser.set_document(doc)
  parser.seek(offset)
  parser.nextobject()
"))

(defun make-pdf-parser (fp)
  (make-instance 'pdf-parser :fp fp))

(defmethod (setf parser-document) (doc (self pdf-parser))
  (setf (slot-value self 'doc) doc))

(defparameter +keyword-r+ (kwd "R"))
(defparameter +keyword-null+ (kwd "null"))
(defparameter +keyword-endobj+ (kwd "endobj"))
(defparameter +keyword-stream+ (kwd "stream"))
(defparameter +keyword-xref+ (kwd "xref"))
(defparameter +keyword-startxref+ (kwd "startxref"))

(defun ensure-integer (x)
  (if (stringp x) (parse-integer x) x))

(defmethod parser-do-keyword ((self pdf-parser) pos token)
  (with-slots (doc fallback fp) self
  (cond ((member token `(,+keyword-xref+ ,+keyword-startxref+))
	 (apply #'parser-add-results self (parser-pop self 1)))
	((eql token +keyword-endobj+)
	 (apply #'parser-add-results self (parser-pop self 4)))
	((eql token +keyword-null+)
	 (parser-push self nil))
	((eql token +keyword-r+)
	 (handler-case
	   (let ((vals (parser-pop self 2)))
	     (destructuring-bind (objid genno)
		 (mapcar (lambda (x) (ensure-integer (cdr x))) vals)
	       (parser-push self (cons pos (make-pdf-obj-ref doc objid genno)))))
	   (ps-syntax-error () nil)))
	((eql token +keyword-stream+)
	 (let
	     ((dic (dict-value (cdar (parser-pop self 1))))
	      (objlen 0)
	      (line))
	   (unless fallback
	     (handler-case
		 (setf objlen (int-value (gethash (lit "Length")  dic)))
	       (key-error ()
		   (when *strict* (error "/Length is undefined: ~s" dic)))))
	   (parser-seek self pos)
	   (handler-case
	       (setf line (cdr (parser-nextline self)))
	     (ps-eof ()
	       (when *strict* (error "Unexpected EOF"))
	       (return-from parser-do-keyword)))
	   (incf pos (length line))
	   (file-position fp pos)
	   (with-output-to-string (data)
	     (write-sequence (pythonic-read fp objlen) data)
	     (parser-seek self (+ pos objlen))
	     (loop
		  for (linepos . line) = (handler-case (parser-nextline self)
					   (ps-eof () (when *strict* (error "Unexpected EOF"))
						   (return)))
		  do (let ((i (cl-ppcre:scan "endstream" line)))
		       (when i
			 (incf objlen i)
			 (write-sequence (subseq line 0 i) data)
			 (return)))
		  (incf objlen (length line))
		  (write-sequence line data))
	     (parser-seek self (+ pos objlen))
	     (parser-push self (cons pos (make-pdf-stream dic (get-output-stream-string data) (slot-value doc 'decipher)))))))
	(t
	 (parser-push self (cons pos token))))))

(defmethod parser-find-xref ((self pdf-parser))
  "Internal function used to locate the first xref"
  (let ((prev nil))
    (loop with generator = (parser-revreadlines self)
	 for line = (funcall generator)
	 while line
	 do (let ((line (strip line)))
	      (when (string= line "startxref") (return))
	      (when (string/= line "")
		(setf prev line)))
	 finally (error (make-condition 'pdf-no-valid-xref "Unexpected EOF")))
    (parse-integer prev)))

(defmethod parser-read-xref-from ((self pdf-parser) start xrefs)
  (parser-seek self start)
  (parser-reset self)
  (destructuring-bind
	(pos . token) 
      (handler-case
	  (parser-nexttoken self)
	(ps-eof () (error (make-condition 'pdf-no-valid-xref "Unexpected EOF"))))
    (let* ((xref
	    (if (typep token 'integer)
		(let ((xref (make-pdf-xref-stream)))
		  (parser-seek self pos)
		  (parser-reset self)
		  (pdf-load xref self *ps-base-parser-debug*)
		  xref)
		(let ((xref (make-pdf-xref)))
		  (when (eql token +keyword-xref+)
		    (parser-nextline self))
		  (pdf-load xref self *ps-base-parser-debug*)
		  xref)))
	   (xrefs (push xref xrefs))
	   (trailer (get-trailer xref)))
      (when (in-dict (lit "XRefStm") trailer)
	(setf xrefs
	      (parser-read-xref-from
	       self
	       (int-value (gethash (lit "XRefStm") trailer))
	       xrefs)))
      (when (in-dict (lit "Prev") trailer)
	(setf xrefs
	      (parser-read-xref-from
	       self
	       (int-value (gethash (lit "Prev") trailer))
	       xrefs)))
      xrefs)))

(defmethod parser-read-xref ((self pdf-parser))
  (handler-case
      (let ((pos (parser-find-xref self)))
	(parser-read-xref-from self pos nil))
    (pdf-no-valid-xref ()
      ;fallback
      (when (<= 1 *ps-base-parser-debug*)
	(format *error-output* "No xref, fallback~%"))
      (setf (slot-value self 'fallback) t)
      (let ((xref (make-pdf-xref)))
	(load-fallback xref self)
	(list xref)))))

(defclass pdf-stream-parser (pdf-parser)
  ()
    (:documentation "
    PDFStreamParser is used to parse PDF content streams
    that is contained in each page and has instructions
    for rendering the page. A reference to a PDF document is
    needed because a PDF content stream can also have
    indirect references to other objects in the same document.
    "))

(defun make-pdf-stream-parser (data)
  (make-instance 'pdf-stream-parser :fp (make-string-input-stream data)))

(defmethod parser-flush ((self pdf-stream-parser))
  (apply #'parser-add-results self (parser-popall self)))

(defmethod parser-do-keyword ((self pdf-stream-parser) pos token)
  (if (eql token +keyword-r+)
      (with-slots (doc) self
	(ignore-errors
	  (let ((vals (parser-pop self 2)))
	    (destructuring-bind
		  (objid genno)
		(mapcar (lambda (x) (parse-integer (cdr x))) vals)
	      (parser-push self (cons pos
				      (make-pdf-obj-ref doc objid genno)))))))
      (parser-push self (cons pos token))))
	
	
(defparameter +pdf-doc-encoding+
  (map 'string
       #'code-char
       (list
	#x0000 #x0001 #x0002 #x0003 #x0004 #x0005 #x0006 #x0007
  #x0008 #x0009 #x000a #x000b #x000c #x000d #x000e #x000f
  #x0010 #x0011 #x0012 #x0013 #x0014 #x0015 #x0017 #x0017
  #x02d8 #x02c7 #x02c6 #x02d9 #x02dd #x02db #x02da #x02dc
  #x0020 #x0021 #x0022 #x0023 #x0024 #x0025 #x0026 #x0027
  #x0028 #x0029 #x002a #x002b #x002c #x002d #x002e #x002f
  #x0030 #x0031 #x0032 #x0033 #x0034 #x0035 #x0036 #x0037
  #x0038 #x0039 #x003a #x003b #x003c #x003d #x003e #x003f
  #x0040 #x0041 #x0042 #x0043 #x0044 #x0045 #x0046 #x0047
  #x0048 #x0049 #x004a #x004b #x004c #x004d #x004e #x004f
  #x0050 #x0051 #x0052 #x0053 #x0054 #x0055 #x0056 #x0057
  #x0058 #x0059 #x005a #x005b #x005c #x005d #x005e #x005f
  #x0060 #x0061 #x0062 #x0063 #x0064 #x0065 #x0066 #x0067
  #x0068 #x0069 #x006a #x006b #x006c #x006d #x006e #x006f
  #x0070 #x0071 #x0072 #x0073 #x0074 #x0075 #x0076 #x0077
  #x0078 #x0079 #x007a #x007b #x007c #x007d #x007e #x0000
  #x2022 #x2020 #x2021 #x2026 #x2014 #x2013 #x0192 #x2044
  #x2039 #x203a #x2212 #x2030 #x201e #x201c #x201d #x2018
  #x2019 #x201a #x2122 #xfb01 #xfb02 #x0141 #x0152 #x0160
  #x0178 #x017d #x0131 #x0142 #x0153 #x0161 #x017e #x0000
  #x20ac #x00a1 #x00a2 #x00a3 #x00a4 #x00a5 #x00a6 #x00a7
  #x00a8 #x00a9 #x00aa #x00ab #x00ac #x0000 #x00ae #x00af
  #x00b0 #x00b1 #x00b2 #x00b3 #x00b4 #x00b5 #x00b6 #x00b7
  #x00b8 #x00b9 #x00ba #x00bb #x00bc #x00bd #x00be #x00bf
  #x00c0 #x00c1 #x00c2 #x00c3 #x00c4 #x00c5 #x00c6 #x00c7
  #x00c8 #x00c9 #x00ca #x00cb #x00cc #x00cd #x00ce #x00cf
  #x00d0 #x00d1 #x00d2 #x00d3 #x00d4 #x00d5 #x00d6 #x00d7
  #x00d8 #x00d9 #x00da #x00db #x00dc #x00dd #x00de #x00df
  #x00e0 #x00e1 #x00e2 #x00e3 #x00e4 #x00e5 #x00e6 #x00e7
  #x00e8 #x00e9 #x00ea #x00eb #x00ec #x00ed #x00ee #x00ef
  #x00f0 #x00f1 #x00f2 #x00f3 #x00f4 #x00f5 #x00f6 #x00f7
  #x00f8 #x00f9 #x00fa #x00fb #x00fc #x00fd #x00fe #x00ff)))

(defun decode-text (s)
  "Decodes a PDFDocEncoding string to Unicode."
  (if (starts-with-subseq (map 'string #'code-char '(#xfe #xff)) s)
      (babel:octets-to-string
       (map '(vector (unsigned-byte 8))
	    #'code-char
	    (subseq s 2))
       :encoding :utf-16be)
      (map 'string
	   (lambda (c)
	     (aref +pdf-doc-encoding+ (char-code c)))
	   s)))

;;;pdfinterp.py
(defparameter +literal-pdf+ (lit "PDF"))
(defparameter +literal-text+ (lit "Text"))
(defparameter +literal-font+ (lit "Font"))
(defparameter +literal-form+ (lit "Form"))
(defparameter +literal-image+ (lit "Image"))

(defclass pdf-text-state ()
  ((font :initform nil)
   (fontsize :initform 0)
   (charspace :initform 0)
   (wordspace :initform 0)
   (scaling :initform 100)
   (leading :initform 0)
   (render :initform 0)
   (rise :initform 0)
   matrix
   linematrix)
  )

(defmethod initialize-instance :after ((self pdf-text-state) &rest initargs)
  (declare (ignorable initargs))
  (reset self))

(defmethod print-object ((self pdf-text-state) stream)
  (with-slots (font fontsize charspace wordspace
		    scaling leading render rise matrix linematrix) self
  (format stream
	  "#<PDF-TEXT-STATE font=~s fontsize=~s charspace=~s wordspace=~s scaling=~s leading=~s render=~s rise=~s matrix =~s linematrix =~s >"
	  font fontsize charspace wordspace scaling leading render rise matrix linematrix)))

;;NB STUB
(defmethod copy ((self pdf-text-state))
  (let ((newobj (make-instance 'pdf-text-state)))
    #-(or)(loop for field in
	 '(font fontsize charspace wordspace
	   scaling leading render rise matrix linematrix)
	 do (setf (slot-value newobj field)
		  (slot-value self field)))
    newobj))

#-(or)(defmethod reset ((self pdf-text-state))
  (with-slots (matrix linematrix) self
    (setf matrix +matrix-identity+
	  linematrix (cons 0 0))))

(defun make-pdf-text-state () (make-instance 'pdf-text-state))

(defmethod reset ((self pdf-text-state))
  (with-slots (matrix linematrix) self
    (setf matrix +matrix-identity+
	  linematrix (list 0 0))))


(defclass pdf-graphic-state ()
  ((linewidth :initform 0)
   (linecap :initform nil)
   (linejoin :initform nil)
   (miterlimit :initform nil)
   (dash :initform nil)
   (intent :initform nil)
   (flatness :initform nil)))

(defmethod copy ((self pdf-graphic-state))
  (let ((obj (make-instance 'pdf-graphic-state)))
    (loop for slot in '(linewidth linecap linejoin miterlimit dash intent flatness)
	 do (setf (slot-value obj slot)
		  (slot-value self slot)))
    obj))

(defun make-pdf-graphic-state () (make-instance 'pdf-graphic-state))

;TODO port repr

(defparameter *pdf-resource-manager-debug* 0)

(defclass pdf-resource-manager ()
  ((caching :initarg :caching)
   (%cached-fonts :initform (make-hash-table)))
  (:documentation "Repository of shared resources.

ResourceManager facilitates reuse of shared resources
such as fonts and images so that large objects are not
allocated multiple times.
")
  (:default-initargs :caching t))

(defun make-pdf-resource-manager (&optional (caching t))
  (make-instance 'pdf-resource-manager :caching caching))

;NB the python get_procset doesn't seem to actually do anything...
(defmethod get-procset ((self pdf-resource-manager) procs)
  (values))

;this doesn't seem to be used anywhere so I won't define yet
;(defmethod get-cmap ((self pdf-resource-manager) cmapname &optional strict)

(defmethod get-font ((self pdf-resource-manager) objid spec)
  (with-slots (%cached-fonts caching) self
  (if (and objid (in-dict objid %cached-fonts))
      (gethash objid %cached-fonts)
      (progn
	(when (<= 2 *pdf-resource-manager-debug*)
	  (format *error-output* "GET-FONT: create objid=~s spec=~s~%"
		  objid spec))
	(when (and *strict*
		   (not (eql (gethash  (lit "Type") spec)
			     +literal-font+)))
	  (error "Type is not /Font"))
	(let ((subtype
	       (gethash (lit "Subtype") spec)))
	  (unless subtype
	    (when *strict*
	      (error "Font Subtype is not specified"))
	    (setf subtype (lit "Type1")))
	  (let
	      ((font
		(case subtype
		  ((ps-literal::|Type1| ps-literal::|MMType1|)
		   (make-pdf-type1-font spec))
		  (ps-literal::|TrueType|
			       (make-pdf-true-type-font spec))
		  (ps-literal::|Type3|
			       (make-pdf-type3-font spec))
		  ((ps-literal::|CIDFontType0| ps-literal::|CIDFontType2|)
		   (make-pdf-cid-font spec))
		  (t
		   (when *strict* (error "Invalid Font spec: ~s" spec))
		   (make-pdf-type1-font spec)))))
	    (when (and objid caching)
	      (setf (gethash objid %cached-fonts) font))
	    font))))))

(defclass pdf-content-parser (ps-stack-parser)
    ((streams :initarg :streams)
     (istream :initform 0)))

(defun make-pdf-content-parser (streams)
  (make-instance 'pdf-content-parser :streams streams :fp nil))

(defmethod parser-fillfp ((self pdf-content-parser))
  (with-slots (fp istream streams) self
  (when (not fp)
    (if
     (< istream (length streams))
     (setf
      fp (make-string-input-stream
	  (get-data (stream-value (elt streams istream ))))
      istream (1+ istream))
     (error (make-condition 'ps-eof "Unexpected EOF, file truncated?"))))))

(defmethod parser-seek ((self pdf-content-parser) pos)
  (parser-fillfp self)
  (call-next-method))

(defmethod parser-fillbuf ((self pdf-content-parser))
  (with-slots (charpos bufpos buf fp) self
    (unless (< charpos (length buf))
      (loop
	 (parser-fillfp self)
	 (setf bufpos (file-position fp)
	       buf (pythonic-read fp +ps-buf-size+))
	 (when (> (length buf) 0)
	   (return))
	 (setf fp nil))
      (setf charpos 0))))

(defmethod parser-get-inline-data ((self pdf-content-parser) pos &optional (target (lit "EI")))
  (let ((target (string target)))
    (with-slots (buf charpos) self
      (parser-seek self pos)
      (let
	  ((data
	    (with-output-to-string (data)
	      (loop
		 with i = 0
		 while (<= i (length target))
		 do (parser-fillbuf self)
		 when (/= i 0)
		 do
		   (let ((c (char buf charpos)))
		     (write-char c data)
		     (incf charpos)
		     (cond
		       ((and (<= (length target) i) (isspace c))
			(incf i))
		       ((and (< i (length target)) (char= c (char target i)))
			(incf i))
		       (t
			(setf i 0))))
		 else
		   do (let ((j (position (char target 0) buf :start charpos)))
		     (if j
			 (progn
			   (write-sequence (subseq buf charpos (1+ j)) data)
			   (setf charpos (1+ j)
				 i 1))
			 (progn
			   (write-sequence (subseq buf charpos) data)
			   (setf charpos (length buf)))))))))
	(cons pos
	      (ppcre:regex-replace
	       "(\\x0d\\x0a|[\\x0d\\x0a])$" ;strip trailing {\r\n,\r,\n}
	       (subseq data 0 (- (length data) (1+ (length target))))
	       ""))))))

(defmethod parser-flush ((self pdf-content-parser))
  (apply #'parser-add-results self (parser-popall self)))

(defparameter +keyword-bi+ (kwd "BI"))
(defparameter +keyword-id+ (kwd "ID"))
(defparameter +keyword-ei+ (kwd "EI"))

(defmethod parser-do-keyword ((self pdf-content-parser) pos token)
  (cond
    ((eql token +keyword-bi+ )
     (parser-start-type self pos :inline))
    ((eql token +keyword-id+)
     (handler-case
	 (let ((objs (cdr (parser-end-type self :inline))))
	   (unless (evenp objs)
	     (error (make-condition 'ps-type-error "Invalid dictionary construct: ~s" objs)))
	   (let*
	       ((d (plist-hash-table objs))
		(v (parser-get-inline-data self (+ pos (length "ID "))))
		(pos (car v))
		(data (cdr v))
		(obj (make-pdf-stream d data)))
	   (parser-push self (cons pos obj))
	   (parser-push self (cons pos +keyword-ei+))))
       (ps-type-error (e)
	 (when *strict* (error e)))))
    (t
     (parser-push self (cons pos token)))))

(defclass pdf-page-interpreter ()
  ((rsrcmgr :initarg :rsrcmgr)
   (device :initarg :device)
   resources
   fontmap
   xobjmap
   gstack
   ctm
   textstate
   graphicstate
   curpath
   argstack
   scs
   ncs
   csmap))

(defparameter *pdf-page-interpreter-debug* 0)

(defun make-pdf-page-interpreter (rsrcmgr device)
  (make-instance 'pdf-page-interpreter :device device :rsrcmgr rsrcmgr))

(defmethod dup ((self pdf-page-interpreter))
  (with-slots (rsrcmgr device) self
    (make-pdf-page-interpreter rsrcmgr device)))

(defmethod init-resources ((self pdf-page-interpreter) %resources)
  (with-slots (resources fontmap xobjmap csmap rsrcmgr) self
    (setf resources %resources
	  fontmap (make-hash-table)
	  xobjmap (make-hash-table)
	  csmap (copy-hash-table +predefined-colorspace+))
    (unless resources
      (flet ((get-colorspace (spec)
	       (let ((name (if (listp spec) (car spec) spec)))
		 (cond
		   ((and (eql name (lit "ICCBased"))
			 (listp spec)
			 (<= 2 (length spec)))
		    (make-pdf-color-space
		     name
		     (getitem (stream-value (second spec)) (lit "N"))))
		   ((and (eql name (lit "DeviceN"))
			 (listp spec)
			 (<= 2 (length spec)))
		    (make-pdf-color-space 
		     name
		     (length (list-value (first spec)))))
		   (t
		    (gethash name +predefined-colorspace+))))))
	(loop for k being the hash-keys of (dict-value resources)
	     using (hash-value v)
	     when (<= 2 *pdf-page-interpreter-debug*)
	     do (format *error-output* "Resource: ~s: ~s~%" k v)
	     when (eql k (lit "Font"))
	     do (loop for font-id being the hash-keys of (dict-value v)
		   using (hash-value spec)
		     for objid = nil
		     when (typep spec 'pdf-obj-ref)
		     do (setf objid (slot-value spec 'objid))
		     (setf (gethash font-id fontmap)
			   (get-font rsrcmgr objid (dict-value spec))))
	     when (eql k (lit "ColorSpace"))
	     do (loop for csid being the hash-keys of (dict-value v)
		     using (hash-value spec)
		     do (setf (gethash csid csmap)
			   (get-colorspace (resolve1 spec))))
	     when (eql k (lit "ProcSet"))
	     do (get-procset rsrcmgr (list-value v))
	     when (eql k (lit "XObject"))
	     do (loop for xobjid being the hash-keys of (dict-value v)
		    using (hash-value xobjstrm)
		    do (setf (gethash xobjid xobjmap) xobjstrm)))))))
		     
(defmethod init-state ((self pdf-page-interpreter) %ctm)
  (with-slots (gstack ctm device textstate graphicstate
		      ncs scs curpath argstack csmap) self
    (setf gstack nil
	  ctm %ctm
	  textstate (make-pdf-text-state)
	  graphicstate (make-pdf-graphic-state)
	  curpath nil
	  argstack nil
	  scs nil
	  ncs nil
	  (device-ctm device) ctm)
    (when csmap
      (setf scs (loop for x being the hash-values of csmap return x)
	    ncs scs))))

(defmethod ipush ((self pdf-page-interpreter) obj)
  (push obj (slot-value self 'argstack)))

(defgeneric ipop (self n))
(defmethod ipop ((self pdf-page-interpreter) n)
  (with-slots (argstack) self
    (assert (>= (length argstack) n))
    (let ((retval nil))
      (loop for i from 1 to n
	   do (push (pop argstack) retval))
      retval)))

(defmethod current-state ((self pdf-page-interpreter))
  (with-slots (ctm textstate graphicstate) self
      (list ctm (copy textstate) (copy graphicstate))))

(defmethod (setf current-state) (state (self pdf-page-interpreter))
  (with-slots (ctm textstate graphicstate device) self
      (setf ctm (first state)
	    textstate (second state)
	    graphicstate (third state)
	    (device-ctm device) ctm)))

(defmacro define-ps-function (fname arglist &body body)
  `(defun ,(intern fname (find-package :ps-keyword))
       ,arglist ,@body))

(define-ps-function "q" (self)
  (with-slots (gstack) self
    (push (current-state self) gstack)))

(define-ps-function "Q" (self)
  (with-slots (gstack) self
    (setf (current-state self)
	  (pop gstack))))

(define-ps-function "cm" (self)
  (with-slots (ctm device) self
      (let ((matrix (ipop self 6)))
	(setf ctm (mult-matrix matrix ctm)
	      (device-ctm device) ctm))))

(defmacro define-ps-gs-func (fname slotname)
  `(define-ps-function ,fname (self)
     (setf
      (slot-value
       (slot-value self 'graphicstate)
       ',slotname)
      (car (ipop self 1)))))

(define-ps-gs-func "w" linewidth)
(define-ps-gs-func "J" linecap)
(define-ps-gs-func "j" linejoin)
(define-ps-gs-func "M" miterlimit)

(define-ps-function "d" (self)
  (setf
   (slot-value
    (slot-value self 'graphicstate)
    'dash)
   (destructuring-bind (dash phase) (ipop self 2)
   (cons
    dash
    phase))))

(define-ps-gs-func "ri" intent)
(define-ps-gs-func "i" flatness)

;XXX
(define-ps-function "gs" (self)
  (car (ipop self 1)))

(define-ps-function "m" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x y) (ipop self 2)
      (setf curpath
	    (nconc curpath
	    (list (list #\m x y)))))))

(define-ps-function "l" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x y) (ipop self 2)
      (setf (cdr (last curpath))
	    (list (list #\l x y))))))

(define-ps-function "c" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x1 y1 x2 y2 x3 y3) (ipop self 6)
      (setf (cdr (last curpath))
	    (list (list #\c x1 y1 x2 y2 x3 y3))))))

(define-ps-function "v" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x2 y2 x3 y3) (ipop self 4)
      (setf (cdr (last curpath))
	    (list (list #\v x2 y2 x3 y3))))))


(define-ps-function "y" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x1 y1 x3 y3) (ipop self 4)
      (setf (cdr (last curpath))
	    (list (list #\y x1 y1 x3 y3))))))

(define-ps-function "h" (self)
  (with-slots (curpath) self
      (setf (cdr (last curpath))
	    (list (list #\h)))))

(define-ps-function "re" (self)
  (with-slots (curpath) self
    (destructuring-bind
	  (x y w h) (ipop self 4)
      (setf curpath
	    (append curpath
		    (list
		     (list #\m x y)
		     (list #\l (+ x w) y)
		     (list #\l (+ x w) (+ y h))
		     (list #\l x (+ y h))
		     (list #\h)
		     ))))))

(define-ps-function "S" (self)
  (with-slots (device graphicstate curpath) self
    (device-paint-path device graphicstate t nil nil curpath)
    (setf curpath nil)))

(define-ps-function "s" (self)
  (funcall (kwd "h") self)
  (funcall (kwd "S") self))

(define-ps-function "f" (self)
  (with-slots (device graphicstate curpath) self
    (device-paint-path device graphicstate nil t nil curpath)
   (setf curpath nil))) 
;obsolete alias for "f"
(define-ps-function "F" (self)
  (funcall (kwd "f") self))

(define-ps-function "f*" (self)
  (with-slots (device graphicstate curpath) self
    (device-paint-path device graphicstate nil t t curpath)
    (setf curpath nil))) 

(define-ps-function "B" (self)
  (with-slots (device graphicstate curpath) self
    (device-paint-path device graphicstate t t nil curpath)
    (setf curpath nil)))

(define-ps-function "B*" (self)
  (with-slots (device graphicstate curpath) self
    (device-paint-path device graphicstate t t t curpath)
    (setf curpath nil)))

(define-ps-function "b" (self)
  (funcall (kwd "h") self)
  (funcall (kwd "B") self))

(define-ps-function "b*" (self)
  (funcall (kwd "h") self)
  (funcall (kwd "B*") self))

(define-ps-function "n" (self)
  (setf (slot-value self 'curpath) nil))

(define-ps-function "W" (self) (declare (ignore self)))
(define-ps-function "W*" (self) (declare (ignore self)))

(define-ps-function "CS" (self)
  (with-slots (scs csmap) self
    (let ((name (car (ipop self 1))))
      (setf scs (gethash name csmap)))))

(define-ps-function "cs" (self)
  (with-slots (ncs csmap) self
    (let ((name (car (ipop self 1))))
      (setf ncs (gethash name csmap)))))

(defmacro define-ps-stub (name nargs)
  `(define-ps-function  ,name (self)
     (when (> ,nargs 0)
       (ipop self ,nargs))))

(define-ps-stub "G" 1)
(define-ps-stub "g" 1)
(define-ps-stub "RG" 3)
(define-ps-stub "rg" 3)
(define-ps-stub "K" 4)
(define-ps-stub "k" 4)

(define-ps-function "SCN" (self)
  (with-slots (scs) self
    (if scs
	(ipop self (slot-value scs 'ncomponents))
	(if *strict* (error "No colorspace specified!")
	    (ipop self 1)))))

(define-ps-function "scn" (self)
  (with-slots (ncs) self
    (if ncs
	(ipop self (slot-value ncs 'ncomponents))
	(if *strict* (error "No colorspace specified!")
	    (ipop self 1)))))

(define-ps-function "SC" (self)
  (funcall (kwd "SCN") self))

(define-ps-function "sc" (self)
  (funcall (kwd "scn") self))

(define-ps-stub "sh" 1)

(define-ps-function "BT" (self)
  (reset (slot-value self 'textstate)))

(define-ps-stub "ET" 0)

(define-ps-stub "BX" 0)
(define-ps-stub "EX" 0)

(define-ps-function "MP" (self)
   (apply
    #'device-do-tag
    (slot-value self 'device)
    (ipop self 1)))

(define-ps-function "DP" (self)
   (apply
    #'device-do-tag
    (slot-value self 'device)
    (ipop self 2)))


(define-ps-function "BMC" (self)
  (apply
   #'device-begin-tag
   (slot-value self 'device)
   (ipop self 1)))

(define-ps-function "BDC" (self)
  (apply
   #'device-begin-tag
   (slot-value self 'device)
   (ipop self 2)))

(define-ps-function "EMC" (self)
  (device-end-tag (slot-value self 'device)))

(define-ps-function "Tc" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'charspace)
   (car (ipop self 1))))

(define-ps-function "Tw" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'wordspace)
   (car (ipop self 1))))

(define-ps-function "Tz" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'scaling)
   (car (ipop self 1))))

(define-ps-function "TL" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'leading)
   (-(car (ipop self 1)))))

(define-ps-function "Tf" (self)
  (with-slots (fontmap textstate) self
  (destructuring-bind
	(fontid fontsize) (ipop self 2)
    (let ((font (gethash fontid fontmap)))
      (if font
	  (setf (slot-value textstate 'font) font)
	  (when *strict* (error "Undefined Font id: ~s" fontid)))
      (setf (slot-value textstate 'fontsize) fontsize)))))

(define-ps-function "Tr" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'render)
   (-(car (ipop self 1)))))

(define-ps-function "Ts" (self)
  (setf
   (slot-value (slot-value self 'textstate) 'rise)
   (-(car (ipop self 1)))))

(define-ps-function "Td" (self)
  (with-slots (matrix linematrix) (slot-value self 'textstate)
    (destructuring-bind
	  (tx ty) (ipop self 2)
      (destructuring-bind (a b c d e f) matrix
	  (setf matrix (list a b c d (+ (* tx a) (* ty c) e) (+ (* tx b) (* ty d) f))
		linematrix (list 0 0))))))

(define-ps-function "TD" (self)
  (with-slots (matrix leading linematrix) (slot-value self 'textstate)
    (destructuring-bind
	  (tx ty) (ipop self 2)
      (destructuring-bind (a b c d e f) matrix
	  (setf matrix (list a b c d (+ (* tx a) (* ty c) e) (+ (* tx b) (* ty d) f))
		leading ty
		linematrix (list 0 0))))))

(define-ps-function "Tm" (self)
  (with-slots (matrix linematrix) (slot-value self 'textstate)
    (setf matrix (ipop self 6)
	  linematrix (list 0 0))))

(define-ps-function "T*" (self)
  (with-slots (matrix leading linematrix) (slot-value self 'textstate)
      (destructuring-bind (a b c d e f) matrix
	(setf matrix (list a b c
			   d (+ (* c leading) e) (+ (* d leading) f))
	      linematrix (list 0 0)))))

(define-ps-function "TJ" (self)
  (with-slots (textstate device) self
  (let ((seq (car (ipop self 1))))
    (unless (or (slot-value (slot-value self 'textstate) 'font)
		(not *strict*))
      (error "No font specified"))
    (device-render-string device textstate seq))))

(define-ps-function "Tj" (self)
  (ipush self (ipop self 1)) ; "TJ" wants a list, ipop returns a list
  (funcall (kwd "TJ") self))

(define-ps-function "'" (self)
  (funcall (kwd "T*") self)
  (ipush self (ipop self 1)) ; "TJ" wants a list, ipop returns a list
  (funcall (kwd "TJ") self))

(define-ps-function "\"" (self)
  (destructuring-bind
	(aw ac s) (ipop self 3)
  (ipush self aw)
  (funcall (kwd "Tw") self)
  (ipush self ac)
  (funcall (kwd "Tc") self)
  (ipush self (list s))
  (funcall (kwd "TJ") self)))

(define-ps-stub "BI" 0)
(define-ps-stub "ID" 0)

(define-ps-function "EI" (self)
  (with-slots (device) self
  (let ((obj (car (ipop self 1))))
    (when (and (contains (lit "W") obj) (contains (lit "H") obj))
      (device-begin-figure device (list 0 0 1 1) +matrix-identity+)
      (device-render-image device obj)
      (device-end-figure device)))))

(define-ps-function "Do" (self)
  (with-slots (xobjmap device ces ctm) self
    (let* ((xobjid (car (ipop self 1)))
	   (xobj (stream-value (gethash xobjid xobjmap ))))
      (if (not xobj)
	  (when *strict* (error "Undefined xobject id: ~s" xobjid))
	  (let ((subtype (getitem xobj (lit "Subtype"))))
	    (cond
	      ((and (eql subtype +literal-form+)
		    (in-dict (lit "BBox") xobj))
	       (let* ((interpreter (dup self))
		      (bbox (list-value (gethash (lit "BBox") xobj)))
		      (matrix (list-value (gethash (lit "Matrix") xobj +matrix-identity+)))
		      (resources (dict-value (gethash (lit "Resources") xobj)))
		      (resources (if (> (hash-table-count resources) 0)
				     resources
				     (copy-hash-table (slot-value self 'resources)))))
				     
		 (device-begin-figure device bbox matrix)
		 (render-contents interpreter
				  resources
				  (list xobj)
				  :ctm (mult-matrix matrix ctm))
		 (device-end-figure device)))
	      ((and (eql subtype +literal-image+)
		    (contains (lit "Width") xobj)
		    (contains (lit "Height") xobj))
	       (device-begin-figure device (list 0 0 1 1) +matrix-identity+)
	       (device-render-image device xobj)
	       (device-end-figure device))
	      (t ;unsupported xobject type
	       nil)))))))

(defmethod process-page ((self pdf-page-interpreter) page)
  (when (<= 1 *pdf-page-interpreter-debug*)
    (format *error-output* "Processsing page: ~s~%" page))
  (with-slots (device) self
  (with-slots (mediabox rotate resources contents) page
    (destructuring-bind (x0 y0 x1 y1) mediabox
      (let ((ctm
	     (case rotate
	       (90
		(list 0 -1 1 0 (- y0) x1))
	       (180
		(list -1 0 0 -1 x1 y1))
	       (270
		(list 0 1 -1 0 y1 (- x0)))
	       (t
		(list 1 0 0 1 (- x0) (- y0))))))
	(device-begin-page device page ctm)
	(render-contents self resources contents :ctm ctm)
	(device-end-page device page))))))

(defmethod render-contents ((self pdf-page-interpreter) resources
			    streams &key (ctm +matrix-identity+))
  (when (<= 1 *pdf-page-interpreter-debug*)
    (format *error-output* "render-contents: resources=~s streams=~s ctm=~s~%"
	    resources streams ctm))
  (init-resources self resources)
  (init-state self ctm)
  (execute self (list-value streams)))

(defmethod execute ((self pdf-page-interpreter) streams)
  (let ((parser
	 (handler-case (make-pdf-content-parser streams)
	   (ps-eof () (return-from execute)))))
    ;(break)
    (loop
	 (let ((obj
		(handler-case (cdr (parser-nextobject parser))
		  (ps-eof () #+(or)(break)(return)))))
	   ;(format *error-output* "OBJ: ~S~%" obj)
	   (if (and (symbolp obj)
		      (eql (symbol-package obj) *ps-keyword-package*))
	       (if (fboundp obj)
		   (funcall obj self)
		   (when *strict* (error "Unknown operator: ~s" obj)))
	       (ipush self obj))))))

(defun make-pdf-input-stream (fname)
  #+sbcl
  (open fname :external-format :iso-8859-1)
  #+ccl
  (open fname :external-format (make-external-format :character-encoding :iso-8859-1 :line-terminatino :unix))
  #-(or sbcl ccl)
  (with-open-file (f fname :element-type '(unsigned-byte 8))
    (make-string-input-stream
     (with-output-to-string (out)
       (loop with bytes = (make-array (* 1024 1024) :element-type '(unsigned-byte 8)
				      (read-sequence bytes f)
				      (write-sequence (map 'list #'code-char bytes) out)))))))
  
(defun process-pdf (rsrcmgr device fp &key (pagenos nil) (maxpages 0)
					(password "") (caching t) (check-extractable t))
  (let*
      ((parser (make-pdf-parser fp))
       (doc (make-pdf-document :caching caching)))
    (setf
       (parser-document parser) doc
       (parser doc) parser)
    (initialize doc password)
    (when (and check-extractable (not (slot-value doc 'is-extractable)))
      (error "Text extraction is not allowed: ~s" fp))
    (let ((interpreter (make-pdf-page-interpreter rsrcmgr device)))
      (loop 
	   with iterator = (get-pages doc)
	 for pageno = 1 then (1+ pageno)
	 for page = (funcall iterator)
	   ;do (format *error-output* "~&PAGE: ~S~%" page)
	 while (and (not (eql page :end))
		    (or (not maxpages) (<= maxpages pageno)))
	 when (or (not pagenos) (member pageno pagenos))
	   do (process-page interpreter page)))))
	   
