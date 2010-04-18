;;; --------------------------------------------------------------------------
;;; tools.lisp
;;;
;;; #Date#: Sun Apr  3 19:41:51 2005
;;;
;;; --------------------------------------------------------------------------
;;;
;;; (C) 2005 Philippe Brochard <pbrochard@common-lisp.net>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
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

(defpackage TOOLS
  (:use COMMON-LISP)
  (:export "CLDEBUG"
	   :fformat
	   :split-string
	   :split-list
	   :aif :it
	   "GET-COMMAND-LINE-WORDS"
	   "STRING-TO-LIST"
	   "NEAR-POSITION"
	   "STRING-TO-LIST-MULTICHAR"
	   "LIST-TO-STRING"
	   "LIST-TO-STRING-LIST"
	   "CLEAN-STRING"
	   "ONE-IN-LIST"
	   "EXCHANGE-ONE-IN-LIST"
	   "ROTATE-LIST"
	   "ROTATE-LIST-REC"
	   "APPEND-FORMATED-LIST"
	   "SHUFFLE-LIST"
	   "PARSE-INTEGER-IN-LIST"
	   "CONVERT-TO-NUMBER"
	   "NEXT-IN-LIST" "PREV-IN-LIST"
	   ;;"TRANSFERT-STREAM"
	   ;;"MY-COPY-FILE"
	   "FIND-STRING"
	   "FIND-ALL-STRINGS"
	   "SUBST-STRINGS"
	   "TEST-FIND-STRING"))


(in-package "TOOLS")


(setq *random-state* (make-random-state t))



(defun cldebug (&rest rest)
  (princ "DEBUG: ")
  (dolist (i rest)
    (princ i))
  (terpri))

(defun fformat (stream formatter &rest args)
  (apply #'format stream formatter args)
  (force-output stream))

(defun split-string (string &optional (separator #\Space))
  "Return a list from a string splited at each separators"
  (loop for i = 0 then (1+ j)
     as j = (position separator string :start i)
     as sub = (subseq string i j)
     unless (string= sub "") collect sub
     while j))


(defun split-list (n list)
  (let (acc1 acc)
    (loop for elem in list
       for i from 1
       do (push elem acc1)
	 (when (zerop (mod i n))
	   (push acc1 acc)
	   (setf acc1 nil)))
    (when acc1
      (push acc1 acc))
    acc))

(defmacro aif (test if else)
  `(let ((it ,test))
     (if it ,if ,else)))



(defun get-command-line-words ()
  #+CLISP ext:*args*
  #+CMU (nthcdr 3 extensions:*command-line-strings*)
  #+SBCL sb-ext:*posix-argv*)



(defun string-to-list (str &key (split-char #\space))
  (do* ((start 0 (1+ index))
	(index (position split-char str :start start)
	       (position split-char str :start start))
	(accum nil))
      ((null index)
       (unless (string= (subseq str start) "")
	 (push (subseq str start) accum))
       (nreverse accum))
    (when (/= start index)
      (push (subseq str start index) accum))))


(defun near-position (chars str &key (start 0))
  (do* ((char chars (cdr char))
	(pos (position (car char) str :start start)
	     (position (car char) str :start start))
	(ret (when pos pos)
	     (if pos
		 (if ret
		     (if (< pos ret)
			 pos
		       ret)
		   pos)
	       ret)))
      ((null char) ret)))


;;;(defun near-position2 (chars str &key (start 0))
;;;  (loop for i in chars
;;;	minimize (position i str :start start)))

;;(format t "~S~%" (near-position '(#\! #\. #\Space #\;) "klmsqk ppii;dsdsqkl.jldfksj lkm" :start 0))
;;(format t "~S~%" (near-position '(#\Space) "klmsqk ppii;dsdsqkl.jldfksj lkm" :start 0))
;;(format t "~S~%" (near-position '(#\; #\l #\m) "klmsqk ppii;dsdsqkl.jldfksj lkm" :start 0))
;;(format t "result=~S~%" (string-to-list-multichar "klmsqk ppii;dsdsqkl.jldfksj lkm" :preserve t))
;;(format t "result=~S~%" (string-to-list-multichar "klmsqk ppii;dsd!sqkl.jldfksj lkm"
;;						  :split-chars '(#\k  #\! #\. #\; #\m)
;;						  :preserve nil))


(defun string-to-list-multichar (str &key (split-chars '(#\space)) (preserve nil))
  (do* ((start 0 (1+ index))
	(index (near-position split-chars str :start start)
	       (near-position split-chars str :start start))
	(accum nil))
      ((null index)
       (unless (string= (subseq str start) "")
	 (push (subseq str start) accum))
       (nreverse accum))
    (let ((retstr (subseq str start (if preserve (1+ index) index))))
      (unless (string= retstr "")
	(push retstr accum)))))





(defun list-to-string (lst)
  (string-trim " () " (format nil "~A" lst)))



(defun clean-string (string)
  "Remove Newline and upcase string"
  (string-upcase
   (string-right-trim '(#\Newline) string)))

(defun one-in-list (lst)
  (nth (random (length lst)) lst))

(defun exchange-one-in-list (lst1 lst2)
  (let ((elem1 (one-in-list lst1))
	(elem2 (one-in-list lst2)))
    (setf lst1 (append (remove elem1 lst1) (list elem2)))
    (setf lst2 (append (remove elem2 lst2) (list elem1)))
    (values lst1 lst2)))


(defun rotate-list (lst &key (time 1))
  (let ((ret lst))
    (dotimes (i time)
      (setq ret (append (cdr ret) (list (car ret)))))
    ret))

(defun rotate-list-rec (lst &key (time 1))
  (if (= time 0) lst
    (rotate-list-rec (append (cdr lst) (list (car lst)))
		     :time (1- time))))


(defun append-formated-list (base-str
			     lst
			     &key (test-not-fun #'(lambda (x) x nil))
			     (print-fun #'(lambda (x) x))
			     (default-str ""))
  (let ((str base-str) (first t))
    (dolist (i lst)
      (cond ((funcall test-not-fun i) nil)
	    (t (setq str
		     (concatenate 'string str
				  (if first "" ", ")
				  (format nil "~A"
					  (funcall print-fun i))))
	       (setq first nil))))
    (if (string= base-str str)
	(concatenate 'string str default-str) str)))


(defun shuffle-list (list &key (time 1))
  "Shuffle a list by swapping elements time times"
  (let ((result (copy-list list))
	(ind1 0) (ind2 0) (swap 0))
    (dotimes (i time)
      (setf ind1 (random (length result)))
      (setf ind2 (random (length result)))

      (setf swap (nth ind1 result))
      (setf (nth ind1 result) (nth ind2 result))
      (setf (nth ind2 result) swap))
    result))



(defun convert-to-number (str)
  (cond ((stringp str) (parse-integer str :junk-allowed t))
	((numberp str) str)))

(defun parse-integer-in-list (lst)
  "Convert all integer string in lst to integer"
  (mapcar #'(lambda (x) (convert-to-number x)) lst))



(defun next-in-list (item lst)
  (do ((x lst (cdr x)))
      ((null x))
    (when (equal item (car x))
      (return (if (cadr x) (cadr x) (car lst))))))

(defun prev-in-list (item lst)
  (next-in-list item (reverse lst)))


;;(defun transfert-stream (in out length &key (bufsize 4096))
;;;;  (ignore-errors
;;    (do* ((data (make-array bufsize
;;			    :element-type (stream-element-type in)))
;;	  (len 0 (read-sequence data in
;;				:start 0
;;				:end (if (> (+ wlen bufsize) length)
;;					 (- length wlen)
;;				       bufsize)))
;;	  (wlen 0 (+ wlen len)))
;;	((>= wlen length) (write-sequence data out :start 0 :end len))
;;      (write-sequence data out :start 0 :end len)));)
;;
;;
;;
;;
;;
;;(defun my-copy-file (in-name out-name)
;;  (with-open-file
;;   (in in-name :direction :input :element-type '(unsigned-byte 8))
;;   (with-open-file
;;    (out out-name :direction :output
;;	 :if-exists :supersede
;;	 :element-type '(unsigned-byte 8))
;;    (transfert-stream in out (file-length in)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;
;;     Find String part.                                            ;;
;;                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun find-string (substr str &key (start 0) (end nil)
			   (test nil) (ignore-case nil))
  "Find substr in str. Return begin and end of substr in str as two values.
Start and end set the findinq region. Ignore-case make find-string case
insensitive.
Test (if needed) must be a function which take str pos1 pos2 and must return
new positions of the substr in str as two values"
  (when (and end (>= start end))
    (return-from find-string nil))
  (let ((pos1 (- start 1))
	(pos2 nil)
	(len (length substr)))
    (when ignore-case
      (setq str (string-upcase str)
	    substr (string-upcase substr)))
    (do ((done nil))
	(done (if (functionp test)
		  (funcall test str pos1 pos2)
		(values pos1 pos2)))
      (setq pos1 (position (aref substr 0) str :start (+ pos1 1) :end end))
      (unless pos1
	(return-from find-string nil))
      (setq pos2 (string>= str substr :start1 pos1 :end1 end))
      (when (and pos2 (= (- pos2 pos1) len))
	(setq done t)))))



(defun find-all-strings (substr str &key (start 0) (end nil)
				(test nil) (ignore-case nil))
  "Find all substr in str. Parameters are the same as find-string.
Return a list with all begin and end positions of substr in str
ie: '((pos1.1 pos1.2) (pos2.1 pos2.2))..."
  (do ((pos (multiple-value-list
	     (find-string substr str :start start :end end
			  :test test :ignore-case ignore-case))
	    (multiple-value-list
	     (find-string substr str :start (second pos) :end end
			  :test test :ignore-case ignore-case)))
       (accum nil))
      ((equal pos '(nil)) (nreverse accum))
    (push pos accum)))



(defun subst-strings (new substr str &key (start 0) (end nil)
			  (test nil) (ignore-case nil))
  "Substitute all substr strings in str with new.
New must be a string or a function witch takes str pos1 pos2
as parameters and return a string to replace substr"
  (let ((outstr (subseq str 0 start))
	(pos1 start)
	(pos2 0)
	(newpos 0))
    (unless end
      (setq end (length str)))
    (do ((done nil))
	(done outstr)
      (multiple-value-setq
	  (pos2 newpos)
	(find-string substr str :start pos1 :end end
		     :test test :ignore-case ignore-case))
      (if pos2
	  (progn
	    (setq outstr (concatenate 'string
				      outstr
				      (subseq str pos1 pos2)
				      (if (functionp new)
					  (funcall new str pos2 newpos)
					new)))
	    (setq pos1 (if (and newpos (<= newpos end))
			   newpos
			 end)))
	(progn
	  (setq outstr (concatenate 'string
				    outstr (subseq str pos1)))
	  (setq done t))))))



(defun my-find-string-test (str pos1 pos2)
  (multiple-value-bind
      (npos1 npos2)
      (find-string "=>" str :start pos2)
    (declare (ignore npos1))
    (values pos1 npos2)))


(defun test-find-string ()
  (let ((count 0)
	(str "bla bla foo <= plop gloup => foo
baz bar <=klm poi => boo <=plop=> faz
lab totrs <= plip =>"))

    (format t "Original:~%~A~2%" str)
    (format t "[1] Simple find on '<=': ~A~%"
	    (multiple-value-list
	     (find-string "<=" str)))
    (format t "[2] Find with start=15/end=50: ~A~%"
	    (multiple-value-list
	     (find-string "<=" str :start 15 :end 50)))

    (format t "[3] Find with test (ie '<=.*=>'): ~A~%"
	    (multiple-value-bind
		(pos1 pos2)
		(find-string "<=" str :test #'my-find-string-test)
	      (subseq str pos1 pos2)))

    (format t "[4] Find all strings: ~A~%"
	    (find-all-strings "<=" str))

    (format t "[5] Find all strings:~%")
    (dolist (pos (find-all-strings "<=" str))
      (format t "Found: ~A~%"
	      (subseq str (car pos) (second pos))))

    (format t "[6] Find all strings with test:~%")
    (dolist (pos (find-all-strings "<=" str :test #'my-find-string-test))
      (format t "Found: ~A~%" (subseq str (car pos) (second pos))))

    (format t "[7] Modifie '<=.*=>' with TOTO:~%~A"
	    (subst-strings "TOTO" "<=" str
			   :test #'my-find-string-test))
    (format t "~%")
    (format t "[8] Modifie '<=.*=>' with a complex expression:~%~A~%"
	    (subst-strings
	     #'(lambda (str pos1 pos2)
		 (let ((repl (string-trim " "
					  (subseq str (+ pos1 2) (- pos2 2)))))
		   (format nil "<=~A:~A (~A)=>"
			   (incf count)
			   repl
			   (reverse repl))))
	     "<=" str
	     :test #'(lambda (str pos1 pos2)
		       (multiple-value-bind
			   (npos1 npos2)
			   (find-string "=>" str :start pos2)
			 (declare (ignore npos1))
			 (values pos1 npos2)))))))


