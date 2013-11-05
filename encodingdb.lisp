(in-package :encoding-db)

;;; Standard encoding tables used in PDF.
;;;
;;; This table is extracted from
;;; pdfminer which in turn extracted it from PDF Reference Manual 1.6 pp.925
;;; ,(lit "D.1 Latin Character Set and Encodings")
;;;

(defparameter +encoding+
  `(
  ; (name std mac win pdf)
  (,(lit "A") 65 65 65 65)
  (,(LIT "AE") 225 174 198 198)
  (,(lit "Aacute") nil 231 193 193)
  (,(lit "Acircumflex") nil 229 194 194)
  (,(lit "Adieresis") nil 128 196 196)
  (,(lit "Agrave") nil 203 192 192)
  (,(lit "Aring") nil 129 197 197)
  (,(lit "Atilde") nil 204 195 195)
  (,(lit "B") 66 66 66 66)
  (,(lit "C") 67 67 67 67)
  (,(lit "Ccedilla") nil 130 199 199)
  (,(lit "D") 68 68 68 68)
  (,(lit "E") 69 69 69 69)
  (,(lit "Eacute") nil 131 201 201)
  (,(lit "Ecircumflex") nil 230 202 202)
  (,(lit "Edieresis") nil 232 203 203)
  (,(lit "Egrave") nil 233 200 200)
  (,(lit "Eth") nil nil 208 208)
  (,(lit "Euro") nil nil 128 160)
  (,(lit "F") 70 70 70 70)
  (,(lit "G") 71 71 71 71)
  (,(lit "H") 72 72 72 72)
  (,(lit "I") 73 73 73 73)
  (,(lit "Iacute") nil 234 205 205)
  (,(lit "Icircumflex") nil 235 206 206)
  (,(lit "Idieresis") nil 236 207 207)
  (,(lit "Igrave") nil 237 204 204)
  (,(lit "J") 74 74 74 74)
  (,(lit "K") 75 75 75 75)
  (,(lit "L") 76 76 76 76)
  (,(lit "Lslash") 232 nil nil 149)
  (,(lit "M") 77 77 77 77)
  (,(lit "N") 78 78 78 78)
  (,(lit "Ntilde") nil 132 209 209)
  (,(lit "O") 79 79 79 79)
  (,(LIT "OE") 234 206 140 150)
  (,(lit "Oacute") nil 238 211 211)
  (,(lit "Ocircumflex") nil 239 212 212)
  (,(lit "Odieresis") nil 133 214 214)
  (,(lit "Ograve") nil 241 210 210)
  (,(lit "Oslash") 233 175 216 216)
  (,(lit "Otilde") nil 205 213 213)
  (,(lit "P") 80 80 80 80)
  (,(lit "Q") 81 81 81 81)
  (,(lit "R") 82 82 82 82)
  (,(lit "S") 83 83 83 83)
  (,(lit "Scaron") nil nil 138 151)
  (,(lit "T") 84 84 84 84)
  (,(lit "Thorn") nil nil 222 222)
  (,(lit "U") 85 85 85 85)
  (,(lit "Uacute") nil 242 218 218)
  (,(lit "Ucircumflex") nil 243 219 219)
  (,(lit "Udieresis") nil 134 220 220)
  (,(lit "Ugrave") nil 244 217 217)
  (,(lit "V") 86 86 86 86)
  (,(lit "W") 87 87 87 87)
  (,(lit "X") 88 88 88 88)
  (,(lit "Y") 89 89 89 89)
  (,(lit "Yacute") nil nil 221 221)
  (,(lit "Ydieresis") nil 217 159 152)
  (,(lit "Z") 90 90 90 90)
  (,(lit "Zcaron") nil nil 142 153)
  (,(lit "a") 97 97 97 97)
  (,(lit "aacute") nil 135 225 225)
  (,(lit "acircumflex") nil 137 226 226)
  (,(lit "acute") 194 171 180 180)
  (,(lit "adieresis") nil 138 228 228)
  (,(lit "ae") 241 190 230 230)
  (,(lit "agrave") nil 136 224 224)
  (,(lit "ampersand") 38 38 38 38)
  (,(lit "aring") nil 140 229 229)
  (,(lit "asciicircum") 94 94 94 94)
  (,(lit "asciitilde") 126 126 126 126)
  (,(lit "asterisk") 42 42 42 42)
  (,(lit "at") 64 64 64 64)
  (,(lit "atilde") nil 139 227 227)
  (,(lit "b") 98 98 98 98)
  (,(lit "backslash") 92 92 92 92)
  (,(lit "bar") 124 124 124 124)
  (,(lit "braceleft") 123 123 123 123)
  (,(lit "braceright") 125 125 125 125)
  (,(lit "bracketleft") 91 91 91 91)
  (,(lit "bracketright") 93 93 93 93)
  (,(lit "breve") 198 249 nil 24)
  (,(lit "brokenbar") nil nil 166 166)
  (,(lit "bullet") 183 165 149 128)
  (,(lit "c") 99 99 99 99)
  (,(lit "caron") 207 255 nil 25)
  (,(lit "ccedilla") nil 141 231 231)
  (,(lit "cedilla") 203 252 184 184)
  (,(lit "cent") 162 162 162 162)
  (,(lit "circumflex") 195 246 136 26)
  (,(lit "colon") 58 58 58 58)
  (,(lit "comma") 44 44 44 44)
  (,(lit "copyright") nil 169 169 169)
  (,(lit "currency") 168 219 164 164)
  (,(lit "d") 100 100 100 100)
  (,(lit "dagger") 178 160 134 129)
  (,(lit "daggerdbl") 179 224 135 130)
  (,(lit "degree") nil 161 176 176)
  (,(lit "dieresis") 200 172 168 168)
  (,(lit "divide") nil 214 247 247)
  (,(lit "dollar") 36 36 36 36)
  (,(lit "dotaccent") 199 250 nil 27)
  (,(lit "dotlessi") 245 245 nil 154)
  (,(lit "e") 101 101 101 101)
  (,(lit "eacute") nil 142 233 233)
  (,(lit "ecircumflex") nil 144 234 234)
  (,(lit "edieresis") nil 145 235 235)
  (,(lit "egrave") nil 143 232 232)
  (,(lit "eight") 56 56 56 56)
  (,(lit "ellipsis") 188 201 133 131)
  (,(lit "emdash") 208 209 151 132)
  (,(lit "endash") 177 208 150 133)
  (,(lit "equal") 61 61 61 61)
  (,(lit "eth") nil nil 240 240)
  (,(lit "exclam") 33 33 33 33)
  (,(lit "exclamdown") 161 193 161 161)
  (,(lit "f") 102 102 102 102)
  (,(lit "fi") 174 222 nil 147)
  (,(lit "five") 53 53 53 53)
  (,(lit "fl") 175 223 nil 148)
  (,(lit "florin") 166 196 131 134)
  (,(lit "four") 52 52 52 52)
  (,(lit "fraction") 164 218 nil 135)
  (,(lit "g") 103 103 103 103)
  (,(lit "germandbls") 251 167 223 223)
  (,(lit "grave") 193 96 96 96)
  (,(lit "greater") 62 62 62 62)
  (,(lit "guillemotleft") 171 199 171 171)
  (,(lit "guillemotright") 187 200 187 187)
  (,(lit "guilsinglleft") 172 220 139 136)
  (,(lit "guilsinglright") 173 221 155 137)
  (,(lit "h") 104 104 104 104)
  (,(lit "hungarumlaut") 205 253 nil 28)
  (,(lit "hyphen") 45 45 45 45)
  (,(lit "i") 105 105 105 105)
  (,(lit "iacute") nil 146 237 237)
  (,(lit "icircumflex") nil 148 238 238)
  (,(lit "idieresis") nil 149 239 239)
  (,(lit "igrave") nil 147 236 236)
  (,(lit "j") 106 106 106 106)
  (,(lit "k") 107 107 107 107)
  (,(lit "l") 108 108 108 108)
  (,(lit "less") 60 60 60 60)
  (,(lit "logicalnot") nil 194 172 172)
  (,(lit "lslash") 248 nil nil 155)
  (,(lit "m") 109 109 109 109)
  (,(lit "macron") 197 248 175 175)
  (,(lit "minus") nil nil nil 138)
  (,(lit "mu") nil 181 181 181)
  (,(lit "multiply") nil nil 215 215)
  (,(lit "n") 110 110 110 110)
  (,(lit "nine") 57 57 57 57)
  (,(lit "ntilde") nil 150 241 241)
  (,(lit "numbersign") 35 35 35 35)
  (,(lit "o") 111 111 111 111)
  (,(lit "oacute") nil 151 243 243)
  (,(lit "ocircumflex") nil 153 244 244)
  (,(lit "odieresis") nil 154 246 246)
  (,(lit "oe") 250 207 156 156)
  (,(lit "ogonek") 206 254 nil 29)
  (,(lit "ograve") nil 152 242 242)
  (,(lit "one") 49 49 49 49)
  (,(lit "onehalf") nil nil 189 189)
  (,(lit "onequarter") nil nil 188 188)
  (,(lit "onesuperior") nil nil 185 185)
  (,(lit "ordfeminine") 227 187 170 170)
  (,(lit "ordmasculine") 235 188 186 186)
  (,(lit "oslash") 249 191 248 248)
  (,(lit "otilde") nil 155 245 245)
  (,(lit "p") 112 112 112 112)
  (,(lit "paragraph") 182 166 182 182)
  (,(lit "parenleft") 40 40 40 40)
  (,(lit "parenright") 41 41 41 41)
  (,(lit "percent") 37 37 37 37)
  (,(lit "period") 46 46 46 46)
  (,(lit "periodcentered") 180 225 183 183)
  (,(lit "perthousand") 189 228 137 139)
  (,(lit "plus") 43 43 43 43)
  (,(lit "plusminus") nil 177 177 177)
  (,(lit "q") 113 113 113 113)
  (,(lit "question") 63 63 63 63)
  (,(lit "questiondown") 191 192 191 191)
  (,(lit "quotedbl") 34 34 34 34)
  (,(lit "quotedblbase") 185 227 132 140)
  (,(lit "quotedblleft") 170 210 147 141)
  (,(lit "quotedblright") 186 211 148 142)
  (,(lit "quoteleft") 96 212 145 143)
  (,(lit "quoteright") 39 213 146 144)
  (,(lit "quotesinglbase") 184 226 130 145)
  (,(lit "quotesingle") 169 39 39 39)
  (,(lit "r") 114 114 114 114)
  (,(lit "registered") nil 168 174 174)
  (,(lit "ring") 202 251 nil 30)
  (,(lit "s") 115 115 115 115)
  (,(lit "scaron") nil nil 154 157)
  (,(lit "section") 167 164 167 167)
  (,(lit "semicolon") 59 59 59 59)
  (,(lit "seven") 55 55 55 55)
  (,(lit "six") 54 54 54 54)
  (,(lit "slash") 47 47 47 47)
  (,(lit "space") 32 32 32 32)
  (,(lit "sterling") 163 163 163 163)
  (,(lit "t") 116 116 116 116)
  (,(lit "thorn") nil nil 254 254)
  (,(lit "three") 51 51 51 51)
  (,(lit "threequarters") nil nil 190 190)
  (,(lit "threesuperior") nil nil 179 179)
  (,(lit "tilde") 196 247 152 31)
  (,(lit "trademark") nil 170 153 146)
  (,(lit "two") 50 50 50 50)
  (,(lit "twosuperior") nil nil 178 178)
  (,(lit "u") 117 117 117 117)
  (,(lit "uacute") nil 156 250 250)
  (,(lit "ucircumflex") nil 158 251 251)
  (,(lit "udieresis") nil 159 252 252)
  (,(lit "ugrave") nil 157 249 249)
  (,(lit "underscore") 95 95 95 95)
  (,(lit "v") 118 118 118 118)
  (,(lit "w") 119 119 119 119)
  (,(lit "x") 120 120 120 120)
  (,(lit "y") 121 121 121 121)
  (,(lit "yacute") nil nil 253 253)
  (,(lit "ydieresis") nil 216 255 255)
  (,(lit "yen") 165 180 165 165)
  (,(lit "z") 122 122 122 122)
  (,(lit "zcaron") nil nil 158 158)
  (,(lit "zero") 48 48 48 48)
))

(defparameter +strip-name+ (cl-ppcre:create-scanner "[0-9]+"))

(defun name2unicode (name)
  (let ((uni (gethash name +glyphname2unicode+)))
    (or uni
	(multiple-value-bind
	      (start end) (cl-ppcre:scan +strip-name+ (symbol-name name))
	  (if start
	      (parse-integer (subseq (symbol-name name) start end))
	      (error (make-condition 'pdfparse::key-error)))))))
	
      

(defparameter *std2unicode* (make-hash-table))
(defparameter *mac2unicode* (make-hash-table))
(defparameter *win2unicode* (make-hash-table))
(defparameter *pdf2unicode* (make-hash-table))

(loop for (name std mac win pdf) in +encoding+
      do (let ((c (name2unicode name)))
	(when std (setf (gethash std *std2unicode*) c))
	(when mac (setf (gethash mac *mac2unicode*) c))
	(when win (setf (gethash win *win2unicode*) c))
	(when pdf (setf (gethash pdf *pdf2unicode*) c))))

(defparameter +encodings+
  (list (lit "StandardEncoding") *std2unicode*
	(lit "MacRomanEncoding") *mac2unicode*
	(lit "WinAnsiEncoding") *win2unicode*
	(lit "PDFDocEncoding") *pdf2unicode*))

(defun get-encoding (name &optional diff)
  (let ((cid2unicode (getf +encodings+ name)))
    (if diff
	(let ((cid2unicode (alexandria:copy-hash-table cid2unicode))
	      (cid 0))
	  (loop for x in diff
	     if (integerp x) do (setf cid 0)
	     else if (symbolp x)
	     do (handler-case
		    (setf (gethash cid cid2unicode)
			  (name2unicode x))
		  (pdfparse::key-error () nil))
	       (incf cid))
	  cid2unicode)
	cid2unicode)))