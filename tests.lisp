(defpackage #:lambda-macro/tests
  (:use :cl)
  (:local-nicknames (#:p #:parachute)))

(in-package #:lambda-macro/tests)

(named-readtables:in-readtable lambda-macro:lambda-macro)

(p:define-test positional-params
  (let* ((no-params (read-from-string "#$(list 1)"))
         (one-param (read-from-string "#$(list $1)"))
         (two-params (read-from-string "#$(list $1 $2)"))
         (skip-param (read-from-string "#$(list $1 $3)")))
    (p:is equal (second no-params) '())
    (p:is equal (second one-param)'($1))
    (p:is equal (second two-params) '($1 $2))
    (p:is equal (second skip-param) '($1 $2 $3))))

(p:define-test optional-params
  (let* ((one-param (read-from-string "#$(list $?1)"))
         (two-params (read-from-string "#$(list $?1 $?2)"))
         (mix-params (read-from-string "#$(list $1 $2 $?1 $?2)"))
         (skip-param (read-from-string "#$(list $1 $2 $?2)")))
    (p:is equal (second one-param)'(&optional $?1))
    (p:is equal (second two-params) '(&optional $?1 $?2))
    (p:is equal (second mix-params) '($1 $2 &optional $?1 $?2))
    (p:is equal (second skip-param) '($1 $2 &optional $?1 $?2))))

(p:define-test keyword-params
  (let* ((one-param (read-from-string "#$(list $a)"))
         (two-params (read-from-string "#$(list $a $b)"))
         (mix-params (read-from-string "#$(list $1 $2 $a $b)"))
         (optional-params (read-from-string "#$(list $1 $2 $?1 $?2 $a $b)"))
         (allow-other-params (read-from-string "#$:(list $1 $2 $a $b)")))
    (p:is equal (second one-param)'(&key a))
    (p:is equal (second two-params) '(&key a b))
    (p:is equal (second mix-params) '($1 $2 &key a b))
    (p:is equal (second optional-params) '($1 $2 &optional $?1 $?2 &key a b))
    (p:is equal (second allow-other-params) '($1 $2 &key a b &allow-other-keys))))

(p:define-test rest-params
  (let* ((only-rest (read-from-string "#$(list $&)"))
         (positional-rest (read-from-string "#$(list $1 $2 $&)"))
         (optional-rest (read-from-string "#$(list $?1 $?2 $&)"))
         (positional-optional-rest (read-from-string "#$(list $1 $2 $?1 $?2 $&)"))
         (key-rest (read-from-string "#$(list $a $b $&)"))
         (positional-key-rest (read-from-string "#$(list $1 $2 $a $b $&)"))
         (positional-optional-key-rest (read-from-string "#$(list $1 $2 $?1 $?2 $a $b $&)"))
         (key-rest-allow-other (read-from-string "#$:(list $a $b $&)"))
         (all-params (read-from-string "#$:(list $1 $2 $?1 $?2 $a $b $&)")))
    (p:is equal (second only-rest) '(&rest $&))
    (p:is equal (second positional-rest) '($1 $2 &rest $&))
    (p:is equal (second optional-rest) '(&optional $?1 $?2 &rest $&))
    (p:is equal (second positional-optional-rest) '($1 $2 &optional $?1 $?2 &rest $&))
    (p:is equal (second key-rest) '(&rest $& &key a b))
    (p:is equal (second positional-key-rest) '($1 $2 &rest $& &key a b))
    (p:is equal (second positional-optional-key-rest) '($1 $2 &optional $?1 $?2
                                                        &rest $& &key a b))
    (p:is equal (second key-rest-allow-other) '(&rest $& &key a b &allow-other-keys))
    (p:is equal (second all-params) '($1 $2 &optional $?1 $?2
                                      &rest $& &key a b &allow-other-keys))))

#-(and)
(progn
  (p:test 'positional-params)
  (p:test 'optional-params)
  (p:test 'keyword-params)
  (p:test 'rest-params)
  )
