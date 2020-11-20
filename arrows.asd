(defsystem #:arrows
  :version "1.0.0"
  :author "Svante von Erichsen <svante.v.erichsen@web.de>"
  :license "CC0"
  :description "An implementation of Clojure-inspired arrow macros."
  :pathname "src"
  :components ((:file "arrows")
               (:file "documentation"))
  :in-order-to ((asdf:test-op (asdf:test-op #:arrows/test))))

(defsystem #:arrows/test
  :version "1.0.0"
  :author "Svante von Erichsen <svante.v.erichsen@web.de>"
  :license "CC0"
  :description "Test suite for Arrows"
  :depends-on (#:arrows #:hu.dwim.stefil)
  :pathname "t"
  :components ((:file "test"))
  :perform (asdf:test-op (c v) (uiop:symbol-call '#:arrows/test
                                                 '#:test-arrows)))
