(in-package :cl-user)

(defpackage fmcs/asdf
  (:nicknames fmcs/sys)
  (:use cl asdf uiop))

(in-package :fmcs/asdf)

(defsystem fmcs
  :description "Flavors Meta-Class System (FMCS) for Demonic Metaprogramming in Common Lisp, an alternative to CLOS+MOP, restored from the CMU AI Repository."
  :author '("\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
            "Jürgen Walther <juergen.walther@gmd.de>"
            "Pierre Cointe"
            "Thomas Christaller"
            "Harry Bretthauer"
            "Jürgen Kopp")
  :maintainer "\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.github.io/fmcs/"
  :source-control (:git "https://github.com/thephoeron/fmcs.git")
  :bug-tracker "https://github.com/thephoeron/fmcs/issues"
  :license "MIT"
  :version (:read-file-form "VERSION")
  :depends-on ((:feature :sbcl fare-quasiquote-extras))
  :serial t
  :components ((:file "package")
               (:file "core")
               (:file "root")
               (:file "methods")
               (:file "util")
               (:file "map")
               (:file "fmcs")))

(defsystem fmcs/test
  :description "Test suite for the FMCS system."
  :author "\"the Phoeron\" Colin J.E. Lupton <thephoeron@protonmail.com>"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.github.io/fmcs/"
  :source-control (:git "https://github.com/thephoeron/fmcs.git")
  :bug-tracker "https://github.com/thephoeron/fmcs/issues"
  :license "MIT"
  :version (:read-file-form "VERSION")
  :depends-on (fmcs)
  :serial t
  :components ((:module "_test"
                :components ((:file "package")
                             (:file "suite")))))
