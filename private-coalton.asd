(defsystem private-coalton
  :name "private-coalton"
  :author "Jason Walker <Jason0@pm.me>"
  :version "0.1"
  :description "A private Coalton utilities/supplements library."
  :depends-on (#:coalton)
  :components ((:file "identity")
               (:file "monad-transformer")
               (:file "reader")
               (:file "testing")))
