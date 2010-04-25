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



(in-package :clwebctrl)


(defun only-head-p (type-request)
  (if (equal type-request :head) :head nil))

(defun find-in-content (string content)
  (when (consp content)
    (second (assoc string content :test #'string-equal))))


(defun is-in-first-pos (search content)
  (zerop (or (search search (string-trim " " content)) -1)))



(defun remove-key (url)
  (print *authorized-keys*)
  (let ((key (second (split-string url #\=))))
    (fformat t "~&****** Remove key: ~A~%" key)
    (setf *authorized-keys* (remove-if #'(lambda (x)
					   (string= (md5 (first x)) key))
				       *authorized-keys*)))
  (print *authorized-keys*))


(defun check-if-identified (sock content)
  (labels ((check-value-with-key (content field key original)
	     (let ((to-check (find-in-content field content)))
	       (when to-check
		 (string= to-check (md5 (concatenate 'string original key)))))))
    (let* ((key (find-in-content "key" content))
	   (client-address (net:client-address sock))
	   (ret (and key
		     (member key *authorized-keys* :test #'(lambda (x y)
							     (and (string= x (first y))
								  (string= client-address (second y)))))
		     (or (and (check-value-with-key content "login" key *login*)
			      (check-value-with-key content "password" key *password*))
			 (check-value-with-key content "identified" key *logged-key*)))))
      (setf *authorized-keys* (remove key *authorized-keys* :test #'(lambda (x y)
								      (string= x (first y)))))
      ret)))


(defmacro with-check-identified ((content sock host only-head) &body body)
  `(if (check-if-identified ,sock ,content)
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
  (let ((auth-key (generate-key)))
    (push (list auth-key (net:client-address sock)) *authorized-keys*)
    (print *authorized-keys*)
    (send-http sock "text/html"
	       (format nil "<html>
<head>
  <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">
  <title>clwebctrl:login</title>
  <script src=\"md5.js\" type=\"text/javascript\"></script>
  <SCRIPT LANGUAGE='JavaScript'>
  <!-- Begin
  function crypt () {
    document.login_form.login.value = hex_md5 (document.login_form.login.value + document.login_form.key.value);
    document.login_form.password.value = hex_md5 (document.login_form.password.value + document.login_form.key.value);
    return true;
  }
  //  End -->
  </script>
</head>
<body onLoad=\"document.login_form.login.focus()\">
  <form action=\"/\" method=\"post\" name=\"login_form\" enctype=\"application/x-www-form-urlencoded\"
        onsubmit=\"return crypt();\">
    <br><br>
    <center>
      <input type=\"hidden\" name=\"key\" value=~S>
      <p> Utilisateur : <input type=\"login\" name=\"login\" id=\"login\"> </p>
      <p> Mot de passe : <input type=\"password\" name=\"password\"> </p>
      <p> <input type=\"submit\" value=\"Envoyer\"> </p>
    </center>  </form>
</body>
</html>"
		       auth-key)
	       only-head)))



(let ((hit 0))
  (defun send-standard-page (sock host content &optional only-head message)
    (with-check-identified (content sock host only-head)
      (let ((auth-key (generate-key)))
	(pushnew (list auth-key (net:client-address sock)) *authorized-keys*)
	(print *authorized-keys*)
	(send-http sock "text/html"
		   (format nil "<html>
<head>
  <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/html4/transitional.dtd\">
  <title>clwebctrl</title>
  <script src=\"md5.js\" type=\"text/javascript\"></script>
  <SCRIPT LANGUAGE='JavaScript'>
  <!-- Begin
  function crypt () {
    document.form.identified.value = hex_md5 (~S + document.form.key.value);
    return true;
  }
  //  End -->
  </script>
</head>
<p align='right'><a href='/&remove_key=~A'>Log Out</a></p>
<body>
  <form action=\"/\" method=\"post\" name=\"form\" enctype=\"application/x-www-form-urlencoded\"
        onsubmit=\"return crypt();\">
    <input type=\"hidden\" name=\"key\" value=~S>
    <input type=\"hidden\" name=\"identified\" value=\"pouf\">
    <p>~A</p>
    ~A
    <hr>
    <p> <a href='/logo.png'>logo</a> </p>
    <p> <img src='/logo.png'> </p>
    ~A
    <p>~A</p>
  </form>
</body>
</html>"
			   *logged-key*
			   (md5 auth-key)
			   auth-key
			   (if message message "")
			   *module-string*
			   (if *in-production* "" "Test code")
			   (incf hit))
		   only-head)))))


(defun send-main-page (sock host content &optional only-head)
  (dolist (action *module-actions*)
    (when (find-in-content (first action) content)
      (funcall (second action) sock host content only-head)
      (return-from send-main-page nil)))
  (send-standard-page sock host content only-head))



(defun send-logo (sock host &optional only-head)
  (declare (ignore host))
  (send-file-http sock "logo.png" :only-head only-head))

(defun send-md5.js (sock host &optional only-head)
  (declare (ignore host))
  (send-file-http sock "src/md5.js" :only-head only-head))


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
      ((or :get :head) (cond ((string-equal "/logo.png" url) (send-logo sock host (only-head-p type-request)))
			     ((string-equal "/md5.js" url) (send-md5.js sock host (only-head-p type-request)))
			     ((search "/&remove_key=" url)
			      (remove-key url)
			      (send-login-page sock host (only-head-p type-request)))
			     (t (send-login-page sock host (only-head-p type-request)))))
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
  (setf *logged-key* (generate-key))
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


