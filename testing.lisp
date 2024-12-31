(in-package :cl-user)
(defpackage :private-coalton.testing
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/state
   #:private-coalton.identity
   #:private-coalton.monad-transformer
   #:private-coalton.environment)
  (:local-nicknames
   (#:m #:coalton-library/ord-map)))
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
  (declare RGB (Env Color (List Integer)))
  (define RGB
    (do
      (color <- ask)
      (pure
        (match color
          ((Red) (make-list 255 0 0))
          ((Blue) (make-list 0 255 0))
          ((Yellow) (make-list 0 0 255)))))))

(cl:defun ex-colors ()
  (coalton
   (run-env
    (do
     (ints-rep <- RGB)
     (let _ = (traceobject "Ints rep" ints-rep))
      (next-color <- (asks rotate))
      (ints-rep2 <- (local (const next-color) RGB))
      (let _ = (traceobject "Ints rep 2" ints-rep2))
      (pure Unit))
    Red)))

;;;
;;; Bank Example
;;;

(coalton-toplevel
  (declare opt->result (:err -> Optional :a -> Result :err :a))
  (define (opt->result failure opt)
    (match opt
      ((None) (Err failure))
      ((Some a) (Ok a)))))

(coalton-toplevel (define-type-alias AccountName String)
  (define-type-alias Balance     Integer)

  (define-struct Configuration
    (minimum-balance      "Minimum balance that must be floated by an account."                          Balance)
    (overdraft-protection "If True, prevents an account from being withdrawn below the minimum balance." Boolean))

  (define-struct Account
    (name    AccountName)
    (balance Balance))

  (define-type-alias BankState (m:Map AccountName Account))

  (define-type-alias BankStateM
    (ST BankState))

  (define-type-alias BankM
    (EnvT Configuration BankStateM))
  
  (declare run-bankM (BankM :val -> Configuration -> BankState -> Tuple BankState :val))
  (define (run-bankM bankm conf initial-balance)
    (run (run-envT bankm conf) initial-balance)))

(coalton-toplevel
  (declare get-account (AccountName -> BankState -> Result String Account))
  (define (get-account account-name accounts)
    (opt->result "Could not find account" (m:lookup accounts account-name))))

(coalton-toplevel
  (declare create-account (AccountName -> Balance -> BankM (Result String Account)))
  (define (create-account name initial-balance)
    (do
     (accounts <- get)
     ()

  (declare deposit (AccountName -> Integer -> BankM (Result String Account)))
  (define (deposit account-name amount)
    (do
      (accounts <- get)
      (let account? = (get-account account-name accounts))
      (sequence (map
                 (fn ((Account name balance))
                   (pure (Account name (+ balance amount))))
                 account?))))

  (declare withdraw (AccountName -> Integer -> BankM (Result String Account)))
  (define (withdraw account-name _amount)
    (do
      (accounts <- get)
      (let _account? = (get-account account-name accounts))
      (_protection? <- (asks .overdraft-protection))
      (_minimum <- (asks .minimum-balance))
      (pure (Err "")))))

(coalton
  (run-bankM
    (do
      (deposit "Checking" 10))
    (Configuration 10 True)
    m:empty))
