;;; --------------------------------------------------------------------------
;;;
;;; (C) 2010 Philippe Brochard <pbrochard@common-lisp.net>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;
;;; --------------------------------------------------------------------------


(in-package "COMMON-LISP-USER")

(defpackage MY-HTTP
  (:use COMMON-LISP COMMON-LISP-USER
	#+(or CLISP CMU) :ext
	#+SBCL :sb-ext
	TOOLS TRANSFER-STREAM)
  (:export "GET-HTTP"
	   "FIND-HTTP-HEADERS"
	   "SEND-HTTP"
	   "SEND-FILE-HTTP"
	   "FIND-FILE-MIME-TYPE"
	   "CLEAN-HTTP-CONTENT"
	   "WRITE-HTTP-CONTENT"))

(in-package "MY-HTTP")




(defvar *http-mime-types*
  '(("html" "text/html; charset=iso-8859-1")
    ("txt"  "text/plain; charset=iso-8859-1")
    ("jpg"  "image/jpeg")
    ("jpeg" "image/jpeg")
    ("gif"  "image/gif")
    ("png"  "image/png")
    ("tif"  "image/tiff")
    ("tiff" "image/tiff")
    ("css"  "text/css")
    ("gz"   "application/octet-stream")
    ("ps"   "application/postscript")
    ("pdf"  "application/pdf")
    ("eps"  "application/postscript")
    ("tar"  "application/x-tar")
    ("rpm"  "application/x-rpm")
    ("zip"  "application/zip")
    ("mp3"  "audio/mpeg")
    ("mp2"  "audio/mpeg")
    ("mid"  "audio/midi")
    ("midi" "audio/midi")
    ("wav"  "audio/x-wav")
    ("au"   "audio/basic")
    ("ram"  "audio/pn-realaudio")
    ("ra"   "audio/x-realaudio")
    ("mpg"  "video/mpeg")
    ("mpeg" "video/mpeg")
    ("qt"   "video/quicktime")
    ("mov"  "video/quicktime")
    ("avi"  "video/x-msvideo")
    ("xul"  "application/vnd.mozilla.xul+xml")
    ("ico"  "image/x-icon")
    ("ogv" "application/ogg")
    ("ogg" "application/ogg")))



(defun parse-a-get/head-url (url type)
  (setq url (string-to-list url :split-char #\?))
  (list type
	(first url)
	(mapcar #'(lambda (x)
		    (string-to-list x :split-char #\=))
		(string-to-list (second url) :split-char #\&))))


(defun parse-a-post-url (sock length)
  (do ((i 0 (1+ i))
       (c)
       (word1)
       (word2)
       (res))
      ((>= i length)
       (when word1
	 (push (coerce (reverse word1) 'string) word2))
       (push (reverse word2) res)
       (list (reverse res)))
    (setq c (read-char sock))
    (case c
      (#\& (when word1
	     (push (coerce (reverse word1) 'string) word2))
	   (push (reverse word2) res)
	   (setq word1 ())
	   (setq word2 ()))
      (#\= (when word1
	     (push (coerce (reverse word1) 'string) word2))
	   (setq word1 ()))
      (t (push c word1)))))






(defun fn-end-write-content (tr)
  (fformat t "~&Write Content: ~A - Socket closed~&"
	   (transfer-name tr))
  (ignore-errors
    (close (transfer-in tr)))
  (ignore-errors
    (close (transfer-out tr))))


(defun write-content-to-file (sock length filename
			      &key (fn-end #'fn-end-write-content))
  (let ((stream (open filename :direction :output
		      :if-exists :supersede
		      :element-type #+CLISP '(unsigned-byte 8) #-CLISP 'character)))
    #+CLISP (setf (stream-element-type sock) '(unsigned-byte 8))
    (add-transfer-stream filename sock stream length :fn-end fn-end)
    (format nil "Output to ~A (~A)" filename length)))



(defun return-content-in-header (sock length)
  (let* ((data (make-array length
			   :element-type #+CLISP '(unsigned-byte 8)
			   #-CLISP 'character)))
    #+CLISP (setf (stream-element-type sock) '(unsigned-byte 8))
    (read-sequence data sock)
    #+CLISP (setf (stream-element-type sock) 'character)
    data))


(defun get-http-content (sock length filename)
  (cond (filename (write-content-to-file sock length filename))
	(t (return-content-in-header sock length))))



(defun read-line-from-sock (sock)
  (let ((line (read-line sock nil :eof)))
    (if (equal line :eof)
	:eof
	(string-trim '(#\return #\linefeed #\newline) line))))


(defun my-file-namestring (filename)
  (subseq filename (max (1+ (or (position #\\ filename :from-end t) -1))
			(1+ (or (position #\/ filename :from-end t) -1)))))




(defun extract-content-from-tempfile (tempfile savedir fn-end-extract-content)
  (let* ((istream (open tempfile :direction :input
			:element-type 'character))
	 (len (file-length istream))
	 (bound-len (length (read-line-from-sock istream)))
	 (savename ""))
    (labels ((fn-end (tr)
	       (fformat t "~&Extract content from tempfile: ~A - Socket closed~&"
			(transfer-name tr))
	       (when fn-end-extract-content
		 (funcall fn-end-extract-content savename))
	       (ignore-errors
		 (close (transfer-in tr)))
	       (ignore-errors
		 (close (transfer-out tr)))
	       (delete-file tempfile)
	       ))
      (do ((line (read-line-from-sock istream)
		 (read-line-from-sock istream)))
	  ((or (eq line :eof) (string= line "")))
	(when (string= (first (string-to-list line)) "Content-Disposition:")
	  (multiple-value-bind
		(pos1 pos2)
	      (find-string "filename=\"" line
			   :test #'(lambda (str pos1 pos2)
				     (when (and pos1 pos2 (< pos1 pos2))
				       (multiple-value-bind
					     (npos1)
					   (find-string "\"" str :start pos2)
					 (values pos2 npos1)))))
	    (when (and pos1 pos2 (< pos1 pos2))
	      (setq savename (my-file-namestring (subseq line pos1 pos2)))))))
      (if (string= savename "")
	  (close istream)
	  (let ((ostream (open (concatenate 'string savedir savename) :direction :output
			       :element-type #+CLISP '(unsigned-byte 8)
			       #-CLISP 'character
			       :if-exists :supersede)))
	    #+CLISP (setf (stream-element-type istream) '(unsigned-byte 8))
	    (add-transfer-stream savename istream ostream
				 (- len bound-len (file-position istream) 6)
				 :fn-end #'fn-end :bufsize 10000))))))





(defun parse-a-post-url-multipart (sock length tempfile savedir
				   fn-end-multipart fn-end-extract-content host)
  (let ((tempfile (format nil"~A~A~A" savedir tempfile (gensym))))
    (labels ((fn-end (tr)
	       (fformat t "~&Parse A Post Url Multipart: ~A - Socket closed~&"
			(transfer-name tr))
	       #+CLISP (setf (stream-element-type (transfer-in tr)) 'character)
	       (when fn-end-multipart
		 (funcall fn-end-multipart (transfer-in tr) host))
	       (ignore-errors
		 (close (transfer-in tr)))
	       (ignore-errors
		 (close (transfer-out tr)))
	       (extract-content-from-tempfile tempfile savedir
					      fn-end-extract-content)))
      (write-content-to-file sock length tempfile :fn-end #'fn-end)
      (list tempfile))))


(defun read-line-list-from-sock (sock)
  (string-to-list (string-trim '(#\return #\linefeed #\newline)
			       (read-line sock))))


(defun get-http (sock &key (filename nil) (tempfile "temp.tmp") (savedir "")
		 (fn-end-multipart nil) (fn-end-extract-content nil))
  "Return a list with http content '(host :GET|:POST|:CONTENT directory content-header.)
When filename is not null then write content to file 'filename'"
  (let (ret
	host
	directory
	(length 0)
	type)
    (do ((head (read-line-list-from-sock sock)
	       (read-line-list-from-sock sock)))
	((null head))
      (case (intern (string-upcase (first head)) "MY-HTTP")
	(GET (setq ret (parse-a-get/head-url (second head) :GET)))
	(HEAD (setq ret (parse-a-get/head-url (second head) :HEAD)))
	(POST (setq ret '(post))
	      (setq directory (second head)))
	(|HOST:| (push (second head) host))
	(|CONTENT-LENGTH:| (setq length (parse-integer (second head))))
	(|CONTENT-TYPE:| (setq type (second head)))))
    (when (eq (first ret) 'post)
      (setq ret (if (string= type "multipart/form-data;")
		    (parse-a-post-url-multipart sock length tempfile savedir
						fn-end-multipart
						fn-end-extract-content
						(first host))
		    (parse-a-post-url sock length)))
      (push directory ret)
      (push :POST ret))
    (unless ret
      (push (get-http-content sock length filename)  ret)
      (push directory ret)
      (push :CONTENT ret))
    (push (car host) ret)
    ret))



(defun find-http-headers (headers str)
  "search a string in headers and return its value"
  (dolist (head (fourth headers))
    (when (string-equal (car head) str)
      (return-from find-http-headers (second head)))))



(defun send-http (sock content-type content &optional only-head)
  (let* ((len (length content))
	 (data #+CLISP (make-array len :element-type '(unsigned-byte 8))
	       #-CLISP content))
    #+CLISP
    (dotimes (i len)
      (setf (aref data i) (char-code (aref content i))))
    (format sock "HTTP/1.1 200 OK
Content-Type: ~A
Connection: close
Content-Length: ~A~2%" content-type len)
    (unless only-head
      #+CLISP (setf (stream-element-type sock) '(unsigned-byte 8))
      (write-sequence data sock)
      #+CLISP (setf (stream-element-type sock) 'character))))


(defun fn-end-send-file (tr)
  (fformat t "~&Send file: ~A - Socket closed~&" (transfer-name tr))
  (ignore-errors
    (close (transfer-in tr)))
  (ignore-errors
    (close (transfer-out tr))))



(defun send-file-http (sock file &key content-type only-head (fn-end #'fn-end-send-file))
  (let* ((stream (open file :direction :input
		       :element-type #+CLISP '(unsigned-byte 8)
		       #-CLISP 'character))
	 (len (file-length stream)))
    (format sock "HTTP/1.1 200 OK
Content-Type: ~A
Connection: close
Content-Length: ~A~2%"
	    (if content-type content-type (find-file-mime-type file))
	    len)
    (unless only-head
      #+CLISP (setf (stream-element-type sock) '(unsigned-byte 8))
      (add-transfer-stream file stream sock len :fn-end fn-end))))


(defun find-file-mime-type (file)
  (let ((ext (car (last (string-to-list file :split-char #\.)))))
    (dolist (i *http-mime-types*)
      (when (string-equal ext (car i))
	(return-from find-file-mime-type (second i)))))
  "text/plain; charset=iso-8859-1")


(defun clean-http-content (content)
  (let ((ret ())
	c)
    (dotimes (i (length content))
      (setq c (aref content i))
      (case c
	(#\+ (setq c #\Space))
	(#\% (setq c (ignore-errors (code-char
				     (parse-integer
				      (format nil "~A~A" (aref content (1+ i))
					      (aref content (+ i 2)))
				      :radix 16))))
	     (incf i 2)
	     (case c
	       (#\Return (setq c nil))
	       (#\Newline (setq c nil)
			  (push #\Linefeed ret)))))
      (when c
	(push c ret)))
    (coerce (nreverse ret) 'string)))


(defun write-http-content (headers filename)
  (unless (eql (second headers) :CONTENT)
    (fformat t "Sorry, headers content no data~%")
    (return-from write-http-content))
  (let* ((content (fourth headers))
	 #+CLISP (len (length content))
	 (data #+CLISP (make-array len :element-type 'character)
	       #-CLISP content))
    #+CLISP
    (dotimes (j len)
      (setf (aref data j) (code-char (aref content j))))
    (with-open-file
	(stream filename :direction :output
		:if-exists :supersede
		:element-type 'character)
      (write-sequence data stream))))

