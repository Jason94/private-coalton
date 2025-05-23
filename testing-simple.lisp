(in-package :cl-user)
(defpackage :private-coalton.testing-simple
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/result
   #:coalton-library/monad/state
   #:coalton-library/monad/environment
   #:coalton-library/monad/resultt)
  (:local-nicknames
   (#:s #:coalton-library/string)
   (#:m #:coalton-library/ord-map)))
(in-package :private-coalton.testing-simple)

(coalton-toplevel
  (define (trace-tuple label-a label-b tup)
    (match tup
      ((Tuple a b)
       (progn
        (traceobject label-a a)
        (traceobject label-b b))))))

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
  (declare RGB (Env Color (List Integer)))
  (define RGB
    (do
      (color <- ask)
      (pure
        (match color
          ((Red) (make-list 255 0 0))
          ((Blue) (make-list 0 255 0))
          ((Yellow) (make-list 0 0 255)))))))

(coalton-toplevel
  (define (ex-colors)
   (run-env
    (do
     (ints-rep <- RGB)
     (let _ = (traceobject "Ints rep" ints-rep))
     (next-color <- (asks-envT rotate))
     (let _ = (traceobject "Next color" next-color))
     (_ints-rep2 <- (local-envT (const next-color) RGB))
      ;; (let _ = (traceobject "Ints rep 2" ints-rep2))
      (pure Unit))
    Red)))

(coalton-toplevel
  (declare print-plus10 (Env Integer Unit))
  (define print-plus10
    (do
     (x <- ask)
     (let _ = (traceobject "X" x))
     (pure Unit))))

(coalton
 (run-env
  (do
   (_ <- print-plus10)
   (_ <- (local-envT (+ 5) print-plus10))
   (pure 1))
  0))
