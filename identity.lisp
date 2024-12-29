(in-package :cl-user)
(defpackage :private-coalton.identity
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes)
  (:export
   #:Identity
   #:run-identity))
(in-package :private-coalton.identity)

(coalton-toplevel
  (repr :transparent)
  (define-type (Identity :a)
    "A bare computation. Not useful on its own, but is useful for running Monad transformers in a bare context."
    (Identity :a))
  
  (declare run-identity (Identity :a -> :a))
  (define (run-identity (Identity a))
    a)

  (define-instance (Functor Identity)
    (define (map fa->b (Identity a))
      (Identity (fa->b a))))

  (define-instance (Applicative Identity)
    (define pure Identity)
    (define (liftA2 fc->d->e (Identity d) (Identity e))
      (Identity (fc->d->e d e))))

  (define-instance (Monad Identity)
    (define (>>= (Identity a) fa->idb)
      (fa->idb a))))