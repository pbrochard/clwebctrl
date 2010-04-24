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

(in-package :common-lisp-user)

(defpackage :clwebctrl
  (:use :common-lisp
	#+(or CLISP CMU) :ext
	#+SBCL :sb-ext
	:tools :my-http :md5
	:transfer-stream
	:shell)
  (:export :start-server))

(in-package :clwebctrl)

(defparameter *in-production* nil)


(defparameter *login* "user")
(defparameter *password* "1234")
(defparameter *port* 8080)

(defparameter *config-file* (merge-pathnames (user-homedir-pathname) ".clwebctrlrc"))


(defparameter *module-list* nil)
(defparameter *module-string* "")
(defparameter *module-actions* nil)

(defstruct module name html-code actions)
