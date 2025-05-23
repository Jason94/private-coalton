(in-package :cl-user)
(defpackage :private-coalton.bank-test
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
(in-package :private-coalton.bank-test)

(coalton-toplevel
  (define (trace-tuple label-a label-b tup)
    (match tup
      ((Tuple a b)
       (progn
        (traceobject label-a a)
        (traceobject label-b b)))))
  
  (declare to-string (Into :a String => :a -> String))
  (define to-string into))

;;;
;;; Bank Example
;;;

(coalton-toplevel
  (define-type-alias AccountName String)
  (define-type-alias Balance Integer)
  (define-type-alias Amount Integer)

  (define-struct Configuration
    (minimum-balance      "Minimum balance that must be floated by an account."                          Balance)
    (overdraft-protection "If True, prevents an account from being withdrawn below the minimum balance." Boolean))

  (declare without-overdraft-protection (Configuration -> Configuration))
  (define (without-overdraft-protection conf)
    (Configuration (.minimum-balance conf) False))

  (define-struct Account
    (name    AccountName)
    (balance Balance))
  
  (declare add-balance (Amount -> Account -> Account))
  (define (add-balance amount acc)
    (Account (.name acc) (+ (.balance acc) amount)))
  
  (declare subtract-balance (Amount -> Account -> Account))
  (define (subtract-balance amount acc)
    (Account (.name acc) (- (.balance acc) amount)))

  (define-type-alias BankState (m:Map AccountName Account))

  (declare print-report (BankState -> Unit))
  (define (print-report accounts)
    (for (Account name balance) in (m:values accounts)
      (lisp :a (name balance)
        (cl:format cl:t "Name:~10T~a~%Balance:~10T~a~%~%" name balance)))
    (lisp :a ()
      (cl:format cl:t "--------~%"))
    Unit)

  (define-type-alias BankStateM
    (ST BankState))

  (define-type-alias BankM
    (EnvT Configuration BankStateM))

  (define-type BankError
    (AccountAlreadyExists  AccountName)
    (InvalidDeposit        Amount)
    (InvalidWithdrawal     Amount)
    (InvalidAccountBalance AccountName Balance)
    (RecursiveTransfer     AccountName)
    (AccountNotFound       AccountName)
    (Unknown               String))

  (define-instance (Into BankError String)
    (define (into err)
      (match err
        ((AccountAlreadyExists name)
         (s:concat "An account already exists with name: " name))
        ((InvalidDeposit _) "Cannot deposit a negative amount")
        ((InvalidWithdrawal _) "Cannot withdraw a negative amount")
        ((InvalidAccountBalance name _)
         (s:concat "Account balance below the minimum balance: "
                   name))
        ((RecursiveTransfer name)
         (s:concat "Cannot transfer between the same account: " name))
        ((AccountNotFound name) (s:concat "Account not found: " name))
        ((Unknown s) (s:concat "Unknown Error: " s)))))

  (define-type-alias BankResult
    (Result BankError))

  (declare run-bankM (BankM :val -> Configuration -> BankState -> Tuple BankState :val))
  (define (run-bankM bankm conf initial-balance)
    (run (run-envT bankm conf) initial-balance)))

(coalton-toplevel
  (declare print-reportM (BankM (BankResult Unit)))
  (define print-reportM
    (do
     (accounts <- get)
     (pure (Ok (print-report accounts)))))

  (declare get-account (AccountName -> BankState -> BankResult Account))
  (define (get-account account-name accounts)
    (opt->result (AccountNotFound account-name) (m:lookup accounts account-name)))

  (declare get-accountM (AccountName -> BankM (BankResult Account)))
  (define (get-accountM account-name)
    (map (get-account account-name) get))

  (declare account-valid? (Account -> BankM (BankResult Account)))
  (define (account-valid? account)
    (do
     (minimum-balance <- (asks-envT .minimum-balance))
     (if (>= (.balance account) minimum-balance)
         (pure (Ok account))
         (pure (Err (InvalidAccountBalance (.name account) (.balance account)))))))

  (declare set-account (Account -> BankM (BankResult Account)))
  (define (set-account acc)
    "Update/insert an account and return it for convenience."
    (do
     (modify (fn (mp)
               (m:insert-or-replace mp (.name acc) acc)))
     (pure (Ok acc)))))

(coalton-toplevel
  (declare create-account (AccountName -> Balance -> BankM (BankResult Account)))
  (define (create-account name initial-balance)
    (run-resultT
     (do
      (accounts <- get)
      (ResultT
       (match (get-account name accounts)
         ((Err _) (pure (Ok Unit)))
         ((Ok _) (pure (Err (AccountAlreadyExists name))))))
      (let unvalidated-account = (Account name initial-balance))
      (account <- (ResultT (account-valid? unvalidated-account)))
      (ResultT (set-account account)))))

  (declare deposit (AccountName -> Amount -> BankM (BankResult Account)))
  (define (deposit account-name amount)
    "Deposit AMOUNT into account with ACCOUNT-NAME and return the Account for convenience."
    (run-resultT
     (do
      (err-ifT (< amount 0) (InvalidDeposit amount))
      (acc <- (ResultT (get-accountM account-name)))
      (ResultT (set-account (add-balance amount acc)))))))

(coalton-toplevel
  (declare withdraw (AccountName -> Amount -> BankM (BankResult Account)))
  (define (withdraw account-name amount)
    "Withdraw AMOUNT from account with ACCOUNT-NAME, returning the Account for convenience."
    (run-resultT
     (do
      (err-ifT (< amount 0) (InvalidWithdrawal amount))
      (acc <- (ResultT (get-accountM account-name)))
      (map-errT (fn (er)
                  (Unknown
                   (s:concat "Cannot withdraw from an invalid account: "
                             (into er))))
                (ResultT (account-valid? acc)))
      (let new-account = (subtract-balance amount acc))
      (protection? <- (lift (asks-envT .overdraft-protection)))
      (minimum <- (lift (asks-envT .minimum-balance)))
      (if (and protection?
               (< (.balance new-account) minimum))
          (ResultT
           (pure (Err (InvalidWithdrawal amount))))
          (ResultT
           (set-account new-account)))))))

(coalton-toplevel
  (declare transfer (AccountName -> AccountName -> Balance -> BankM (BankResult Unit)))
  (define (transfer from-acc-name to-acc-name amount)
    (if (== from-acc-name to-acc-name)
      (pure (Err (RecursiveTransfer from-acc-name)))
      (do
        (withdrawal? <- (withdraw from-acc-name amount))
        (match withdrawal?
          ((Err er) (pure (Err er)))
          ((Ok _)
            (do
            (deposit? <- (deposit to-acc-name amount))
            (match deposit?
              ;; If the deposit failed, put the money back into the from account!
              ((Err er)
                (do
                (deposit from-acc-name amount)
                (pure (Err er))))
              ((Ok _) (pure (Ok Unit))))))))))
)

(coalton-toplevel

  (declare close-account (AccountName -> AccountName -> BankM (BankResult Unit)))
  (define (close-account acc-to-close-name deposit-acc-name)
    (run-resultT
     (do
      (acc-to-close <- (ResultT (get-accountM acc-to-close-name)))
      (ResultT (local-envT
                without-overdraft-protection
                (transfer acc-to-close-name deposit-acc-name (.balance acc-to-close))))
      (modify (fn (mp)
                (with-default
                  mp
                  (m:remove mp acc-to-close-name))))
      (pure Unit)))))

(cl:defmacro do-resultT (cl:&body body)
  `(run-resultT
    (do
     ,@(cl:mapcar
        (cl:lambda (form)
          `(ResultT ,form))
        body))))

(coalton
  (progn
    (let (Tuple accounts res) =
      (run-bankM
       (do-resultT
         (create-account "Checking" 100)
         (create-account "Savings" 50)
         (deposit "Savings" 20)
         (withdraw "Checking" 15)
         print-reportM
         (close-account "Checking" "Savings")
         (create-account "Reserves" 10)
         print-reportM
         (deposit "Reserves" 15)
         (transfer "Checking" "Reserves" 40)
         )
       (Configuration 10 True)
       m:empty))
    (print-report accounts)
    (traceobject "Result" (map-err to-string res))))
