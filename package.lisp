(in-package :cl-user)

(defpackage fmcs
  (:nicknames flavors-metaclass-system flavors)
  (:use cl)
  (:shadow #:defclass
           #:defmethod
           #:make-instance
           #:slot-value
           #:standard-object
           #:standard-class
           #:self
           #:call-next-method)
  (:export #:*redefine-warnings*
           #:self
           #:$slot 
           #:def$flavor
           #:def$method
           #:undef$method 
           #:def$frame
           #:def$behavior
           #:trace$method
           #:untrace$method
           #:is-traced$method
           #:compile-$flavor-$methods 
           #:defwhopper
           #:continue-whopper 
           #:$send
           #:lexpr-$send
           #:flavorp
           #:flavor-instancep
           #:flavor-typep
           #:flavor-type-of
           #:get-flavor-instance-slots
           #:symbol-value-in-$instance 
           #:make-$instance
           #:make-window-or-instance
           #:mcs-trace
           #:mcs-untrace
           #:mcs-is-traced))

(in-package :fmcs)

;; Switch to the FARE-QUASIQUOTE readtable on SBCL

#+sbcl
(named-readtables:in-readtable :fare-quasiquote)