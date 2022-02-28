(asdf:defsystem #:lambda-macro
  :description "A small reader macro that provides a Clojure-like lambda reader macro"
  :author "Volodymyr Ivanov <me@funcall.me>"
  :license "BSD-3-Clause"
  :version "1.0.1"
  :depends-on (#:named-readtables)
  :serial t
  :components ((:file "lambda-macro")))

(asdf:defsystem #:lambda-macro/tests
  :description "Tests for the lambda-macro system"
  :author "Volodymyr Ivanov <me@funcall.me>"
  :license "BSD-3-Clause"
  :version "1.0.1"
  :depends-on (#:lambda-macro #:named-readtables #:parachute)
  :serial t
  :components ((:file "tests")))
