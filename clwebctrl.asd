;;;; -*- Mode: Lisp -*-
;;;; Author: Philippe Brochard <pbrochard@common-lisp.net>
;;;; ASDF System Definition

(in-package #:asdf)

(defsystem clwebctrl
  :description "Control a machine from a web browser"
  :version "0.0"
  :author "Philippe Brochard  <pbrochard@common-lisp.net>"
  :licence "GNU General Public License (GPL)"
  :components ((:module :src
			:components
			((:file "tools")
			 (:file "shell")
			 (:file "net")
			 (:file "md5")
			 (:file "transfer-stream"
				:depends-on ("tools" "net"))
			 (:file "my-http"
				:depends-on ("tools" "net" "transfer-stream"))
			 (:file "package"
				:depends-on ("tools" "net" "my-http" "transfer-stream" "shell" "md5"))
			 (:file "module"
				:depends-on ("tools" "net" "my-http" "transfer-stream" "shell" "package"))
			 (:file "clwebctrl"
				:depends-on ("tools" "net" "my-http" "transfer-stream" "shell" "package" "module" "md5"))))))




