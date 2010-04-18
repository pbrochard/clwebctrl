(in-package :cl-user)

(defpackage :shell
  (:export :sh :sh-no-wait)
  (:use :common-lisp))

(in-package :shell)

;;; Shell part
(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))


(defun urun-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  #+gcl (declare (ignore wait))
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp      lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl))
  (if wait
      (apply #'lisp:run-program prog :arguments args opts)
      (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait :output *standard-output* opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system-showing-output
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :wait wait :output *standard-output* opts)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))


(defun sh (&rest args)
  (urun-prog "/bin/sh" :args (list "-c" (format nil "~{~A ~}" args))))

(defun sh-no-wait (&rest args)
  (urun-prog "/bin/sh" :args (list "-c" (format nil "~{~A ~}" args)) :wait nil))

