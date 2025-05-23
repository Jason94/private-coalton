(in-package :cl-user)
(defpackage :private-coalton.conts
  (:use
   #:coalton
   #:coalton-prelude
   #:cl-cont))
(in-package :private-coalton.conts)

(cont:with-call/cc
  (coalton-toplevel
    (declare foo-wrapped (Unit -> String))
    (define (foo-wrapped)
      (trace "Begin foo")
      (cont:call/cc 
        (cl:lambda ()
          (cl:format t "Before cc'ing from foo ~%")))
      "Foo return value")))

(coalton (foo-wrapped))