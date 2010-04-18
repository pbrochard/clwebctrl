;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <pbrochard@common-lisp.net>
;;;; ASDF System Definition
;;;
;;; #date#: Fri Mar 25 21:56:40 2005

(in-package #:asdf)

(defsystem clwebctrl
  :description "Server: control a server from a web browser"
  :version "1.0"
  :author "Philippe Brochard  <pbrochard@common-lisp.net>"
  :licence "GNU General Public License (GPL)"
  :components ((:module :src
			:components
			((:file "tools")
			 (:file "shell")
			 (:file "net")
			 (:file "transfer-stream"
				:depends-on ("tools" "net"))
			 (:file "my-http"
				:depends-on ("tools" "net" "transfer-stream"))
			 (:file "clwebctrl"
				:depends-on ("tools" "net" "my-http" "transfer-stream" "shell"))))))




