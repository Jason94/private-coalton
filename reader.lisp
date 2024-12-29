(in-package :cl-user)
(defpackage :private-coalton.reader
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/functions
   #:coalton-library/classes)
  (:export
   #:Reader
   #:run-reader
   #:local
   #:ask
   #:asks))
(in-package :private-coalton.reader)

(coalton-toplevel
  (repr :transparent)
  (define-type (Reader :env :value)
    "A computation that runs inside an :env environment."
    (Reader (:env -> :value)))
  
  (declare run-reader (Reader :env :value -> :env -> :value))
  (define (run-reader (Reader fenv->val) env)
    "Run a Reader inside an environment."
    (fenv->val env))
  
  (declare local ((:env -> :env) -> Reader :env :value -> Reader :env :value))
  (define (local fenv (Reader fenv->a))
    "Run a computation in a modified environment."
    (Reader (compose fenv->a fenv)))
  
  (declare ask (Reader :env :env))
  (define ask
    "Retrieve the computation environment."
    (Reader id))
  
  (declare asks ((:env -> :a) -> Reader :env :a))
  (define asks
    "Retrieve an aspect of the computation environment."
    Reader))

(coalton-toplevel
  (define-instance (Functor (Reader :env))
    (define (map fa->b (Reader fenv->a))
      (Reader (compose fa->b fenv->a))))
  
  (define-instance (Applicative (Reader :env))
    (define pure (compose Reader const))
    (define (liftA2 fc->d->e (Reader fenv->c) (Reader fenv->d))
      (Reader (fn (env)
                (fc->d->e (fenv->c env) (fenv->d env))))))
  
  (define-instance (Monad (Reader :env))
    (define (>>= (Reader fenv->a) fa->readerb)
      (Reader (fn (env)
                (match (fa->readerb (fenv->a env))
                  ((Reader fenv->b)
                   (fenv->b env))))))))

(coalton-toplevel
  (repr :enum)
  (define-type Color Red Blue Yellow)

  (declare rotate (Color -> Color))
  (define (rotate c)
    (match c
      ((Red) Blue)
      ((Blue) Yellow)
      ((Yellow) Red)))

  (declare RGB (Reader Color (List Integer)))
  (define RGB
    (do
      (color <- ask)
      (pure
        (match color
          ((Red) (make-list 255 0 0))
          ((Blue) (make-list 0 255 0))
          ((Yellow) (make-list 0 0 255)))))))

(coalton
  (run-reader
    (do
      (ints-rep <- RGB)
      (let _ = (traceobject "Ints rep" ints-rep))
      (next-color <- (asks rotate))
      (ints-rep2 <- (local (const next-color) RGB))
      (let _ = (traceobject "Ints rep 2" ints-rep2))
      (pure Unit))
    Red))

(coalton
  (run-reader
    (Reader (+ 1))
    0))