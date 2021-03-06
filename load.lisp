;;; --------------------------------------------------------------------------
;;;
;;; (C) 2010 Philippe Brochard <hocwp@free.fr>
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


#+SBCL (require :asdf)

#-ASDF
(let* ((base-dir (directory-namestring *load-truename*))
       (asdf-pathname (make-pathname :host (pathname-host base-dir)
				     :device (pathname-device base-dir)
				     :directory (pathname-directory base-dir)
				     :name "asdf.lisp")))
  (load asdf-pathname))


(push (directory-namestring *load-truename*) asdf:*central-registry*)

(asdf:oos 'asdf:load-op :clwebctrl)

(clwebctrl:start-server)

(quit)
