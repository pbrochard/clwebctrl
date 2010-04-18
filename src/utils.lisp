;;; --------------------------------------------------------------------------
;;;
;;; #Date#: Wed Mar 30 23:33:48 2005
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

(in-package :common-lisp-user)

(defpackage UTILS
  (:use COMMON-LISP TOOLS MY-HTTP)
  (:export "GET-DIRECTORY-CONTENT"
	   "SEND-DIRECTORY-CONTENT"
	   "GET-DIRECTORY-CONTENT-FROM-HTTP"
	   "STRING-UNIVERSAL-TIME"
	   "MERGE-CONTENT-DIRECTORY"))

(in-package "UTILS")



(defun get-directory-content (&optional (file "*"))
  (let ((content-rep (remove nil (mapcar #'file-namestring (directory file)))))
    ;; remove directory and backup file from list
    (dolist (i content-rep)
      (when (or (eql #\/ (aref (namestring i) (1- (length (namestring i)))))
		(eql #\~ (aref (namestring i) (1- (length (namestring i))))))
	(format t "Remove: ~A~%" i)
	(setq content-rep (remove i content-rep))))
    (setq content-rep
	  (mapcar #'(lambda (x)
		      (list x (file-write-date x)))
		  content-rep))
;;    (format t "Update content: ~A~%" content-rep)
    content-rep))



(defun send-directory-content (sock content)
  (send-http sock "text/plain; charset=iso-8859-1"
	     (with-output-to-string
	       (str)
	       (dolist (i content)
		 (format str "~A ~A~%" (car i) (second i))))))
;;		 (format str "~A~%" (namestring i))))))



(defun get-directory-content-from-http (sock)
  (let ((headers (get-http sock)))
    (mapcar #'(lambda (x)
		(let ((c (string-to-list x)))
		  (list (car c)
			(ignore-errors
			  (parse-integer (second c))))))
	    (string-to-list
	     (with-output-to-string
	       (str)
	       (dolist (i (coerce (fourth headers) 'list))
		 (format str "~c"
			 #+CLISP (code-char i)
			 #-CLISP i))
	       (format str "~%"))
	     :split-char #\Newline))))


(defun string-universal-time (&optional (utime (get-universal-time)))
  (cond ((eql utime :NEW) "New")
	((numberp utime )
	 (multiple-value-bind (s min h d m y)
	     (decode-universal-time utime)
	   (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
		   y m d h min s)))
	(t "New")))


(defun merge-content-directory (remote local)
  "Take two list in the form '((\"filename\" file-write-date) ...)
And return a list which merge remote and local content in
the form '((\"filename\" file-write-date-remote file-write-date-local) ...)
file-write-date-remote is :New if filename exists on localhost and not
in remote host.
file-write-date-local is :New if filename exists on remote host and not
in local host."
  (let (merge
	found)
    (dolist (i remote)
      (setq found nil)
      (dolist (j local)
	(when (string= (car i) (car j))
	  (setq found j)))
      (when found
	(push (list (car i) (second i) (second found)) merge)
	(setq remote (remove i remote))
	(setq local (remove found local))))
    (dolist (i remote)
      (push (list (car i) (second i) :new) merge))
    (dolist (i local)
      (push (list (car i) :new (second i)) merge))
    merge))
