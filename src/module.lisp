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

(in-package :clwebctrl)

(defun defmodule (name html-code &rest actions)
  (push (make-module :name name :html-code html-code :actions actions)
	*module-list*))

(defun add-in-module-string (str)
  (setf *module-string* (format nil "~A~A~%" *module-string* str)))

(defun use-module (&rest name-list)
  (add-in-module-string "<p>")
  (dolist (name name-list)
    (dolist (module *module-list*)
      (when (equal name (module-name module))
	(add-in-module-string (module-html-code module))
	(dolist (action (module-actions module))
	  (pushnew action *module-actions*)))))
  (add-in-module-string "</p>"))




(defun send-webcam (sock host content &optional only-head)
  (declare (ignore host content))
  (sh "camE -f -s")
  (send-file-http sock "/tmp/webcam.jpg" :only-head only-head))


(defun send-ssh-enable (sock host content &optional only-head)
  (send-standard-page sock host content only-head "SSH enabled"))

(defun send-ssh-disable (sock host content &optional only-head)
  (send-standard-page sock host content only-head "SSH disabled"))


(defun send-halt-server-confirm (sock host content &optional only-head)
  (send-standard-page sock host content only-head "Really shutdown the server ?
<input type='submit' name='halt_server_confirm' value='Yes'>"))

(defun send-halt-server (sock host content &optional only-head)
  (send-standard-page sock host content only-head "Server Halted!")
  (when *in-production*
    (sh "sudo halt")))


(defmodule 'refresh
    "<input type='submit' name='refresh' value='refresh'>")

(defmodule 'shutdown-server
    "<input type='submit' name='halt_server' value='Shutdown Server'>"
  '("halt_server" send-halt-server-confirm)
  '("halt_server_confirm" send-halt-server))

(defmodule 'manage-ssh
    "<input type='submit' name='ssh_enable' value='Enable SSH'>
       <input type='submit' name='ssh_disable' value='Disable SSH'>"
  '("ssh_enable" send-ssh-enable)
  '("ssh_disable" send-ssh-disable))


(defmodule 'webcam
    "<input type='submit' name='webcam' value='webcam'>"
  '("webcam" send-webcam))
