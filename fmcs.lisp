(in-package :fmcs)

;; If everything compiles and loads correctly, then we can PROVIDE :FMCS
;; and add it to the *FEATURES* list.

(provide :fmcs)

(pushnew :fmcs *features*)
