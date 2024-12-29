(cl:in-package :cl-user)
(defpackage :private-coalton.io/term
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:private-coalton.io/monad)
  (:export
   #:write-line-str))
(in-package :private-coalton.io/term)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare write-line-std ((Into :a String) => :a -> IO Unit))
  (define (write-line-std obj)
    (let str = (the String (into obj)))
    (IO (fn ()
          (lisp :a (str)
            (cl:format cl:t "~a~%" str))
          Unit))))
