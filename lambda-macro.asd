(asdf:defsystem #:lambda-macro
  :description "A small reader macro that provides a Clojure-like lambda reader macro"
  :author "Volodymyr Ivanov <me@funcall.me>"
  :license "BSD-3-Clause"
  :version "1.0.1"
  :depends-on (#:named-readtables)
  :serial t
  :components ((:file "lambda-macro")))
