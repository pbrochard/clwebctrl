;;; --------------------------------------------------------------------------
;;; transfer-stream : transfer sequences over stream
;;;
;;; #Date#: Wed Mar 30 23:33:43 2005
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

(defpackage :transfer-stream
  (:use :common-lisp :tools)
  (:export :add-transfer-stream
	   :transfer-tick
	   :transfer-copy-file
	   :transfer-name :transfer-in :transfer-out
	   :init-transfer-stream
	   :have-to-close-socket
	   :transfer-end-p))


(in-package :transfer-stream)



(defvar *transfer-list* ())
(defvar *have-to-close* t)

(defvar *max-zero-len-count* 1000)


(defstruct transfer name in out wlen length bufsize data fn-end zero-len-count)



(defun init-transfer-stream ()
  (setq *have-to-close* t))

(defun have-to-close-socket ()
  *have-to-close*)

(defun transfer-end-p ()
  (null *transfer-list*))




(defun default-fn-end (tr)
  (fformat t "~&~A : End of file and Stream closed~%" (transfer-name tr))
  (ignore-errors
    (close (transfer-in tr)))
  (ignore-errors
    (close (transfer-out tr))))



(defun create-transfer-stream (name in out length &key (bufsize 4096) (fn-end #'default-fn-end))
  (make-transfer :name name :in in :out out :wlen 0 :length length
		 :bufsize bufsize
		 :data (make-array bufsize
				   :element-type (stream-element-type in))
		 :fn-end fn-end
		 :zero-len-count 0))


(defun add-transfer-stream (name in out length &key (bufsize 4096) (fn-end #'default-fn-end))
  (setq *have-to-close* nil)
  (fformat t "~&Add to transfer-stream: ~A~&" name)
  (push (create-transfer-stream name in out length :bufsize bufsize :fn-end fn-end)
	*transfer-list*))


(defun transfer-stream-tick (tr)
  (let ((len (read-sequence (transfer-data tr) (transfer-in tr)
			    :start 0
			    :end (if (> (+ (transfer-wlen tr) (transfer-bufsize tr))
					(transfer-length tr))
				     (- (transfer-length tr) (transfer-wlen tr))
				     (transfer-bufsize tr)))))
    (write-sequence (transfer-data tr) (transfer-out tr) :start 0 :end len)
    (incf (transfer-wlen tr) len)
    (if (zerop len)
	(incf (transfer-zero-len-count tr))
	(setf (transfer-zero-len-count tr) 0))
    (cond ((>= (transfer-wlen tr) (transfer-length tr)) :eof)
	  ((> (transfer-zero-len-count tr) *max-zero-len-count*)
	   (fformat t "~&Transfer-Tick : Too zero length count~&")
	   :eof))))



(defun transfer-tick ()
  (dolist (tr *transfer-list*)
    (multiple-value-bind (result error)
	(ignore-errors
	  (when (eql (transfer-stream-tick tr) :eof)
	    (when (transfer-fn-end tr)
	      (funcall (transfer-fn-end tr) tr))
	    (setq *transfer-list* (remove tr *transfer-list*))))
      (declare (ignore result))
      (when error
	(fformat t "~&Error in transfer-tick: ~A~&"
		 (transfer-name tr))
	(ignore-errors
	  (funcall (transfer-fn-end tr) tr))
	(setq *transfer-list* (remove tr *transfer-list*))))))



(defun transfer-copy-file (in-name out-name)
  (with-open-file
      (in in-name :direction :input :element-type '(unsigned-byte 8))
    (with-open-file
	(out out-name :direction :output
	     :if-exists :supersede
	     :element-type '(unsigned-byte 8))
      (let ((tr (create-transfer-stream "copy file" in out (file-length in))))
	(loop until (transfer-stream-tick tr))))))

