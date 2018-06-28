(defsystem #:arrows
  :name "arrows"
  :version "0.2.0"
  :author "Svante von Erichsen <svante.v.erichsen@web.de>"
  :license "CC0" 
  :description
  "Implements -> and ->> from Clojure, as well as several expansions on the
idea."
  :components ((:file "arrows"))
  :in-order-to ((asdf:test-op (asdf:test-op #:arrows/test))))

(defsystem #:arrows/test
  :depends-on (#:arrows #:hu.dwim.stefil)
  :components ((:file "test"))
  :perform (asdf:test-op (c v) (uiop:symbol-call '#:arrows/test
                                                 '#:test-arrows)))
