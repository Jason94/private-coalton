(in-package :cl-user)
(defpackage :private-coalton.testing
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/state
   #:private-coalton.identity
   #:private-coalton.monad-transformer
   #:private-coalton.reader))
(in-package :private-coalton.testing)

(coalton-toplevel
  (repr :enum)
  (define-type Color Red Blue Yellow)

  (declare rotate (Color -> Color))
  (define (rotate c)
    (match c
      ((Red) Blue)
      ((Blue) Yellow)
      ((Yellow) Red))))

(coalton-toplevel
  (declare RGB (ReaderT Color Identity (List Integer)))
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

(coalton-toplevel
  (define-struct Configuration
    (overdraft-protection Boolean))

  (define-type-alias (BankState :val)
    (ST Integer :val))

  (define-type-alias (BankM :val)
    (ReaderT Configuration (ST Integer) :val))
  
  (declare run-bankM (BankM :val -> Configuration -> Integer -> Tuple Integer :val))
  (define (run-bankM bankm conf initial-balance)
    (run (run-readerT bankm conf) initial-balance)))
  
(coalton-toplevel
  (declare deposit (Integer -> BankM Unit))
  (define (deposit amount)
    (do
      (balance <- (lift get))
      (lift (put (+ amount balance)))))
  
  (declare withdraw (Integer -> BankM Integer))
  (define (withdraw amount)
    (do
      (balance <- (lift get))
      (protection? <- (asks .overdraft-protection))
      (lift
        (if (or (>= balance amount) (not protection?))
          (do
            (put (- balance amount))
            (pure amount))
          (pure 0))))))

(coalton
  (run-bankM
    (do
      (deposit 10)
      (withdraw 20))
    (Configuration True)
    0))

  ; (define-type-alias (BankM2 :val)
  ;   (ReaderT Configuration (ST Integer :val) :val)))