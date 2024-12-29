(in-package :cl-user)
(defpackage :private-coalton.monad-transformer
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/classes)
  (:export
   #:MonadTransformer
   #:lift))
(in-package :private-coalton.monad-transformer)

(coalton-toplevel
  (define-class (MonadTransformer :t)
    (lift (Monad :m => :m :a -> :t :m :a))))