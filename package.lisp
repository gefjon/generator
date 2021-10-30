(uiop:define-package :generator/package
  (:nicknames :generator)
  ;; we can't `:use' `:cl' becuase these packages shadow a bunch of its symbols, and we don't `:mix' because i
  ;; don't trust how that `define-package' keyword works.
  (:use-reexport
   :generator/defs :generator/produce :generator/consume :generator/combine))
(cl:in-package :generator/package)
