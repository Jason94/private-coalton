(in-package :cl-user)
(defpackage :private-coalton.reader
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/classes
   #:private-coalton.identity
   #:private-coalton.monad-transformer)
  (:export
   #:ReaderT
   #:run-readerT
   #:Reader
   #:run-reader
   #:local
   #:ask
   #:asks
   #:lift-readerT))
(in-package :private-coalton.reader)

(coalton-toplevel
  (repr :transparent)
  (define-type (ReaderT :env :m :value)
    "A monadic computation that runs inside an :env environment."
    (ReaderT (:env -> :m :value)))
  
  (define-type-alias (Reader :env :value) (ReaderT :env Identity :value))
  
  (declare run-readerT (ReaderT :env :m :value -> :env -> :m :value))
  (define (run-readerT (ReaderT fenv->val) env)
    "Run a ReaderT inside an environment."
    (fenv->val env))
  
  ;; TODO: This works, but waiting on a fix to the alias PR

  ; (declare run-reader (Reader :env :value -> :env -> :value))
  (declare run-reader (ReaderT :env Identity :value -> :env -> :value))
  (define (run-reader reader env)
    "Run a Reader inside an environment."
    (run-identity (run-readerT reader env)))
  
  (declare local ((:env -> :env) -> ReaderT :env :m :value -> ReaderT :env :m :value))
  (define (local fenv (ReaderT fenv->a))
    "Run a computation in a modified environment."
    (ReaderT (compose fenv->a fenv)))
  
  (declare ask (Monad :m => ReaderT :env :m :env))
  (define ask
    "Retrieve the computation environment."
    (ReaderT (compose pure id)))
  
  (declare asks (Applicative :m => (:env -> :a) -> ReaderT :env :m :a))
  (define (asks fenv->a)
    "Retrieve an aspect of the computation environment."
    (ReaderT (compose pure fenv->a))))

(coalton-toplevel
  (declare map-readerT ((:m :a -> :n :b) -> ReaderT :env :m :a -> ReaderT :env :n :b))
  (define (map-readerT fma->nb (ReaderT fenv->ma))
    (ReaderT (compose fma->nb fenv->ma)))
  
  (declare lift-readerT (:m :a -> ReaderT :env :m :a))
  (define lift-readerT (compose ReaderT const)))

(coalton-toplevel
  (define-instance (Functor :m => Functor (ReaderT :env :m))
    (define map (compose map-readerT map)))
  
  (define-instance (Applicative :m => Applicative (ReaderT :env :m))
    (define pure (compose lift-readerT pure))
    (define (liftA2 fc->d->e (ReaderT fenv->mc) (ReaderT fenv->md))
      (ReaderT (fn (env)
                (liftA2 fc->d->e (fenv->mc env) (fenv->md env))))))
  
  (define-instance (Monad :m => Monad (ReaderT :env :m))
    (define (>>= (ReaderT fenv->ma) fa->readermb)
      (ReaderT
        (fn (env)
          (>>= (fenv->ma env)
               (fn (a)
                 (match (fa->readermb a)
                   ((ReaderT fenv->mb)
                    (fenv->mb env)))))))))

  (define-instance (MonadTransformer (ReaderT :env))
    (define lift lift-readerT)))

