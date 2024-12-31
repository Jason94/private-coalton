(in-package :cl-user)
(defpackage :private-coalton.testing
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
(in-package :private-coalton.testing)

(coalton-toplevel
  (define (trace-tuple label-a label-b tup)
    (match tup
      ((Tuple a b)
       (progn
        (traceobject label-a a)
        (traceobject label-b b))))))

;;;
;;; Bank Example
;;;

(coalton-toplevel (define-type-alias AccountName String)
  (define-type-alias Balance Integer)

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
    (opt->result "Could not find account" (m:lookup accounts account-name)))

  (declare account-valid? (Account -> BankM (Result String Account)))
  (define (account-valid? account)
    (do
     (minimum-balance <- (asks .minimum-balance))
     (if (>= (.balance account) minimum-balance)
         (pure (Ok account))
         (pure (Err "Account balance is too low!")))))

  (declare set-account (Account -> BankM (Result String Account)))
  (define (set-account acc)
    "Update/insert an account and return it for convenience."
    (do
     (modify (fn (mp)
               (m:insert-or-replace mp (.name acc) acc)))
     (pure (Ok acc)))))

(coalton-toplevel
  (declare create-account (AccountName -> Balance -> BankM (Result String Account)))
  (define (create-account name initial-balance)
    (run-resultT
     (do
      (accounts <- get)
      (ResultT
       (match (get-account name accounts)
         ((Err _) (pure (Ok Unit)))
         ((Ok _) (pure (Err "Account already exists")))))
      (let unvalidated-account = (Account name initial-balance))
      (account <- (ResultT (account-valid? unvalidated-account)))
      (ResultT (set-account account)))))

  (declare deposit (AccountName -> Integer -> BankM (Result String Account)))
  (define (deposit account-name amount)
    "Deposit AMOUNT into account with ACCOUNT-NAME and return the Account for convenience."
    (run-resultT
     (do
      (err-ifT (< amount 0) "Cannot deposit negative amount")
      ((Account _ balance) <- (ResultT (map (get-account account-name) get)))
      (let new-account = (Account account-name (+ balance amount)))
      (ResultT (set-account new-account))))))

(coalton-toplevel
  (declare withdraw (AccountName -> Integer -> BankM (Result String Account)))
  (define (withdraw account-name amount)
    "Withdraw AMOUNT from account with ACCOUNT-NAME, returning the Account for convenience."
    (run-resultT
     (do
      (err-ifT (< amount 0) "Cannot withdraw negative amount")
      (acc <- (ResultT (map (get-account account-name) get)))
      (map-errT (fn (msg)
                  (s:concat "Cannot withdraw from an invalid account ["
                            (s:concat msg "]")))
                (ResultT (account-valid? acc)))
      (let new-account = (Account account-name (- (.balance acc) amount)))
      (protection? <- (lift (asks .overdraft-protection)))
      (minimum <- (lift (asks .minimum-balance)))
      (if (and protection?
               (< (.balance new-account) minimum))
          (ResultT (pure (Err "Cannot withdraw below minimum balance with overdraft protection.")))
          (pure Unit))
      (ResultT (set-account new-account))))))

(coalton
 (trace-tuple
  "Accounts" "Result"
  (run-bankM
   (run-resultT
   (do
    (ResultT (create-account "Checking" 100))
    (ResultT (deposit "Checking" 10))
    (ResultT (withdraw "Checking" 20))
    (ResultT (withdraw "Checking" 85))
    (ResultT (deposit "Checking" 2))
    (ResultT (withdraw "Checking" 5))
     ))
   (Configuration 10 False)
   m:empty)))
