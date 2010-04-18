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





;;(declaim (optimize (speed 3)
;;		   (compilation-speed 0)
;;		   (safety 0)
;;		   (debug 0)
;;		   (space 1)))


(in-package :common-lisp-user)

(defpackage :clwebctrl
  (:use :common-lisp
	#+(or CLISP CMU) :ext
	#+SBCL :sb-ext
	:tools :my-http
	:transfer-stream
	:shell)
  (:export :start-server))


(in-package :clwebctrl)


(defparameter *login* "user")
(defparameter *password* "1234")
(defparameter *port* 8080)

(defparameter *config-file* (merge-pathnames (user-homedir-pathname) ".clwebctrlrc"))



(defun only-head-p (type-request)
  (if (equal type-request :head) :head nil))

(defun find-in-content (string content)
  (when (consp content)
    (second (assoc string content :test #'string-equal))))


(defun is-in-first-pos (search content)
  (zerop (or (search search (string-trim " " content)) -1)))


(defun check-if-identified (content)
  (or (and (equal (find-in-content "login" content) *login*)
	   (equal (find-in-content "password" content) *password*))
      (equal (find-in-content "identified" content) "identified")))

(defmacro with-check-identified ((content sock host only-head) &body body)
  `(if (check-if-identified ,content)
       ,@body
       (send-login-page ,sock ,host ,only-head)))




(defun send-basic-page (sock host title string url-return string-return
			     &optional (only-head nil))
  (send-http sock "text/html"
	     (format nil "<html>
<head>
  <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">
  <title>~A</title>
</head>
<body>
  ~A<br>
  <a href=\"~A\">~A</a><br><br>
  <small><a href=\"http://~A\">http://~A</a></small>
</body>
</html>" title string url-return string-return host host)
	     only-head))


(defun send-login-page (sock host &optional (only-head nil))
  (declare (ignore host))
  (send-http sock "text/html"
	     (format nil "<html>
<head>
  <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">
  <title>clwebctrl:login</title>
</head>
<body onLoad=\"document.login_form.login.focus()\">
  <form action=\"/\" method=\"post\" name=\"login_form\" enctype=\"application/x-www-form-urlencoded\">
    <br><br>
    <center>
      <p> Utilisateur : <input type=\"login\" name=\"login\" id=\"login\"> </p>
      <p> Mot de passe : <input type=\"password\" name=\"password\"> </p>
      <p> <input type=\"submit\" value=\"Envoyer\"> </p>
    </center>  </form>
</body>
</html>")
	     only-head))


(let ((plop 0))
  (defun send-standard-page (sock host content &optional only-head message)
    (with-check-identified (content sock host only-head)
      (send-http sock "text/html"
		 (format nil "<html>
<head>
  <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">
  <title>clwebctrl:login</title>
</head>
<p align='right'><a href='/'>Log Out</a></p>
<p>~A</p>
<body onLoad=\"document.login_form.login.focus()\">
  <form action=\"/\" method=\"post\" name=\"login_form\" enctype=\"application/x-www-form-urlencoded\">
    <input type=\"hidden\" name=\"identified\" value=\"identified\">
    <p> <a href='/logo.png'>logo</a> </p>
    <p> <img src='/logo.png'> </p>
    <p><input type='submit' name='webcam' value='webcam'></p>
    <p><input type='submit' name='ssh_enable' value='Enable SSH'>
       <input type='submit' name='ssh_disable' value='Disable SSH'></p>
    <p><input type='submit' name='halt_server' value='Halt Server'></p>
    <p><input type='submit' name='refresh' value='refresh'></p>
    <p>~A</p>
  </form>
</body>
</html>"
			 (if message message "")
			 (incf plop))
		 only-head))))


(defun send-main-page (sock host content &optional only-head)
  (cond ((find-in-content "webcam" content) (send-webcam sock host only-head))
	((find-in-content "ssh_enable" content) (send-ssh 'enabled sock host content only-head))
	((find-in-content "ssh_disable" content) (send-ssh 'disabled sock host content only-head))
	((find-in-content "halt_server" content) (send-halt-server sock host content only-head))
	(t (send-standard-page sock host content only-head))))



(defun send-logo (sock host &optional only-head)
  (declare (ignore host))
  (send-file-http sock "logo.png" :only-head only-head))

(defun send-webcam (sock host &optional only-head)
  (declare (ignore host))
  (sh "camE -f -s")
  (send-file-http sock "/tmp/webcam.jpg" :only-head only-head))


(defun send-ssh (action sock host content &optional only-head)
  (case action
    (enabled (print 'enabled))
    (disabled (print 'disabled)))
  (send-standard-page sock host content only-head (format nil "SSH ~A!" action)))


(defun send-halt-server (sock host content &optional only-head)
  (send-standard-page sock host content only-head "Server Halted!")
  (sh "sudo halt"))



(defun fn-end-multipart (sock host)
  (send-main-page sock host nil))




(defun fn-end-extract-content (filename)
  (format t "~&File saved from client: ~A~&" filename))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main request handler part  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun send-page (sock headers)
  (let ((host (first headers))
	(type-request (second headers))
	(url (third headers))
	(content (fourth headers)))
    (case type-request
      ((or :get :head) (cond ((string-equal url "/") (send-login-page sock host (only-head-p type-request)))
			     ((string-equal "/logo.png" url) (send-logo sock host (only-head-p type-request)))
			     (t (fformat t "Unknown address on GET~%")
				(send-basic-page sock host
						 "Unknown page"
						 "Sorry, this page is not on server.<br>" "" ""
						 (only-head-p type-request)))))
      (:post (cond ((string-equal url "/") (send-main-page sock host content (only-head-p type-request)))
		   (t (fformat t "Unknown address on POST~%")
		      (send-basic-page sock host
				       "Unknown page"
				       "Sorry, this page is not on server.<br>" "" ""
				       (only-head-p type-request))))))))



(defun wait-connexion (server-sock)
  (let ((sock (net:socket-accept server-sock :wait 0.01d0)))
    (when sock
      (init-transfer-stream)
      (fformat t "~&******************** New Connexion ********************~%")
      (unwind-protect
	  (let ((headers (get-http sock :tempfile "temp.tmp" :savedir "/tmp/"
				   :fn-end-multipart #'fn-end-multipart
				   :fn-end-extract-content #'fn-end-extract-content)))
	    (fformat t "=> Reading headers : ~S~%" headers)
	    (send-page sock headers))
	(when (have-to-close-socket)
	  (fformat t "~&Socket closed manually~%")
	  (close sock))))))




(defun server-loop (server-sock)
  (catch 'quit
    (unwind-protect
	(loop
	   (handler-case
	       (progn
		 (wait-connexion server-sock)
		 (transfer-tick))
	     (error (c)
	       (fformat t "Error: ~A~%" c))))
      (net:socket-server-close server-sock)
      (fformat t "~&~%Socket closed...~%"))))



(defun start-server (&optional (port *port*))
  (when (probe-file *config-file*)
    (load *config-file*))
  (let ((server-sock (net:open-socket-server port)))
    (fformat t "~&~%Start server on port ~A~%" port)
    (fformat t "~&~%You can watch the directory content with a web browser pointed to~%")
    (fformat t "   http://localhost:~A~%" port)
    (fformat t "or http://your_ip:~A~%" port)
    (when (or (equal *login* "user") (equal *password* "password"))
      (error "Error: You are using the default login and/or password.
Please, change this settings in the clwebctrl configuration file:
Add this lines in ~A:
-----------------------------------------------
(in-package :clwebctrl)

(defparameter *login* \"your_login_name\")
(defparameter *password* \"your_login_password\")
-----------------------------------------------"
*config-file*))
    (server-loop server-sock)))


