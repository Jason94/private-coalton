(cl:in-package :cl-user)
(defpackage :trampoline
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/tuple
   #:coalton-library/functions
   #:coalton-library/monad/state)
  (:local-nicknames
   (#:c   #:coalton-library/cell)
   (#:itr #:coalton-library/iterator)))
(in-package :trampoline)

(named-readtables:in-readtable coalton:coalton)

(cl:declaim #.coalton-impl/settings:*coalton-optimize*)

;; Bad functions
(coalton-toplevel
  (declare zip-index-bad (List :a -> List (Tuple Integer :a)))
  (define (zip-index-bad lst)
    (reverse
     (snd
      (run
       (fold
        (fn (accSt elt)
          (do
           (acc <- accSt)
           (idx <- get)
            (put (1+ idx))
            (pure (Cons (Tuple idx elt) acc))))
        (pure Nil)
        lst)
       0)))))

;; (coalton
;;  (zip-index-bad (range 0 100000)))

(coalton-toplevel
  (declare bad-even (Integer -> Boolean))
  (define (bad-even n)
    (if (zero? n)
        True
        (bad-odd (1- n))))

  (declare bad-odd (Integer -> Boolean))
  (define (bad-odd n)
    (if (zero? n)
        False
        (bad-even (1- n)))))

(coalton
 (bad-odd 11))

;; (coalton
;;  (bad-odd 1000000))

;;;
;;; 3/4 - Trampolines
;;;

(coalton-toplevel
  (define-type (Trampoline :a :b)
    (More    (Unit -> Trampoline :a :b))
    (Done    :a)
    (Binding (Trampoline :a :b) (:a -> Trampoline :a :b)))

  (declare resume (Trampoline :a :b -> (Result (Unit -> Trampoline :a :b) :a)))
  (define (resume t)
    (match t
      ((Done a)     (Ok a))
      ((More f->ta) (Err f->ta))
      ((Binding ta fa->tb)
       (match ta
          ((Done a)     (resume (fa->tb a)))
          ((More f->ta) (Err (fn () (Binding (f->ta) fa->tb))))
          ((Binding ta2 fa->tb2)
           (resume (Binding ta2
                            (fn (a)
                              (Binding (fa->tb2 a)
                                       fa->tb)))))))))

  (declare runT (Trampoline :a :b -> :a))
  (define (runT t)
    (match (resume t)
      ((OK a)      a)
      ((Err f->ta) (runT (f->ta)))))

  ;; (define-instance (Functor (Trampoline :a))
  ;;   (define (map ))
  )

(coalton-toplevel
  (declare even (Integer -> Trampoline Boolean Unit))
  (define (even n)
    (if (zero? n)
        (Done True)
        (More (fn () (odd (1- n))))))

  (declare odd (Integer -> Trampoline Boolean Unit))
  (define (odd n)
    (if (zero? n)
        (Done False)
        (More (fn () (even (1- n)))))))

(coalton-toplevel
 (define test-n 500000000))

(cl:defun test-trampoline ()
  (cl:time
   (coalton (runT (even test-n)))))

;;;
;;; Supp - Iterative
;;;

(coalton-toplevel
  (declare even-iter (Integer -> Boolean))
  (define (even-iter n)
    (let ret = (c:new True))
    (for x in (itr:range-decreasing 1 n 0)
      (when (zero? x)
        (return (c:read ret)))
      (c:write! ret (not (c:read ret))))
    (error "Unreachable")))

(cl:defun test-iter ()
  (cl:time
   (coalton (even-iter test-n))))

(cl:defun cl-even-iter (n)
  (cl:declare (cl:fixnum n))
  (cl:let ((ret cl:t))
    (cl:dotimes (i n)
      (cl:when (cl:eql i n)
        (cl:return ret))
      (cl:setf ret (cl:not ret)))))

(cl:defun test-cl-iter ()
  (cl:time (cl-even-iter (coalton test-n))))
