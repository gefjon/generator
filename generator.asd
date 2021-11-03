(defsystem "generator"
  :author "Phoebe Goldman <phoebe@goldman-tribe.org>"
  :version "0.0.1"
  :class :package-inferred-system
  :depends-on ("generator/package")
  :in-order-to ((test-op (test-op "generator/test"))))

(defsystem "generator/test"
  :defsystem-depends-on ((:version "fiveam-asdf" "3.0.1"))
  :class :package-inferred-fiveam-tester-system
  :depends-on ("generator/test/package")
  :test-names ((#:generator-suite . :generator/test/package)))
