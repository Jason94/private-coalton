(in-package :cl-user)
(defpackage :private-coalton.environment
  ;; (:use
  ;;  #:coalton
  ;;  #:coalton-prelude
  ;;  #:coalton-library/functions
  ;;  #:coalton-library/classes
  ;;  #:coalton-library/monad/state
  ;;  #:private-coalton.identity
  ;;  #:private-coalton.monad-transformer)
  ;; (:export
  ;;  #:EnvT
  ;;  #:run-envT
  ;;  #:Env
  ;;  #:run-env
  ;;  #:local
  ;;  #:ask
  ;;  #:asks
  ;;  #:lift-envT))
  )
(in-package :private-coalton.environment)

;; (coalton-toplevel
;;   (repr :transparent)
;;   (define-type (EnvT :env :m :value)
;;     "A monadic computation that runs inside an :env environment."
;;     (EnvT (:env -> :m :value)))

;;   (define-type-alias (Env :env :value) (EnvT :env Identity :value))

;;   (declare run-envT (EnvT :env :m :value -> :env -> :m :value))
;;   (define (run-envT (EnvT fenv->val) env)
;;     "Run a EnvT inside an environment."
;;     (fenv->val env))

;;   (declare run-env (Env :env :value -> :env -> :value))
;;   (define (run-env env-computation env)
;;     "Run a Env inside an environment."
;;     (run-identity (run-envT env-computation env)))

;;   (declare local ((:env -> :env) -> EnvT :env :m :value -> EnvT :env :m :value))
;;   (define (local fenv (EnvT fenv->a))
;;     "Run a computation in a modified environment."
;;     (EnvT (compose fenv->a fenv)))

;;   (declare ask (Monad :m => EnvT :env :m :env))
;;   (define ask
;;     "Retrieve the computation environment."
;;     (EnvT (compose pure id)))

;;   (declare asks (Applicative :m => (:env -> :a) -> EnvT :env :m :a))
;;   (define (asks fenv->a)
;;     "Retrieve an aspect of the computation environment."
;;     (EnvT (compose pure fenv->a))))

;; (coalton-toplevel
;;   (declare map-envT ((:m :a -> :n :b) -> EnvT :env :m :a -> EnvT :env :n :b))
;;   (define (map-envT fma->nb (EnvT fenv->ma))
;;     (EnvT (compose fma->nb fenv->ma)))

;;   (declare lift-envT (:m :a -> EnvT :env :m :a))
;;   (define lift-envT (compose EnvT const)))

;; (coalton-toplevel
;;   (define-instance (Functor :m => Functor (EnvT :env :m))
;;     (define map (compose map-envT map)))

;;   (define-instance (Applicative :m => Applicative (EnvT :env :m))
;;     (define pure (compose lift-envT pure))
;;     (define (liftA2 fc->d->e (EnvT fenv->mc) (EnvT fenv->md))
;;       (EnvT (fn (env)
;;                 (liftA2 fc->d->e (fenv->mc env) (fenv->md env))))))

;;   (define-instance (Monad :m => Monad (EnvT :env :m))
;;     (define (>>= (EnvT fenv->ma) fa->envmb)
;;       (EnvT
;;         (fn (env)
;;           (>>= (fenv->ma env)
;;                (fn (a)
;;                  (match (fa->envmb a)
;;                    ((EnvT fenv->mb)
;;                     (fenv->mb env)))))))))

;;   (define-instance (MonadTransformer (EnvT :env))
;;     (define lift lift-envT)))

;; (coalton-toplevel
;;   (define-instance (MonadState :s :m => MonadState :s (EnvT :env :m))
;;     (define put (compose lift put))
;;     (define get (lift get))))
