;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-

(asdf:defsystem :chunga
  :serial t
  :version "1.1.7"
  :depends-on (:trivial-gray-streams)
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "known-words")
               (:file "conditions")
               (:file "read")
               (:file "streams")
               (:file "input")
               (:file "output")))
