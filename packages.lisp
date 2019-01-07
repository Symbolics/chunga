;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CL-USER; Base: 10 -*-

(in-package :cl-user)

(defpackage :chunga
  (:use :cl :trivial-gray-streams)
  #+:lispworks
  (:import-from :lw :when-let)
  (:export :*accept-bogus-eols*
           :*current-error-message*
           :*treat-semicolon-as-continuation*
           :assert-char
           :as-keyword
           :as-capitalized-string
           :chunga-error
           :chunga-warning
           :chunked-input-stream
           :chunked-input-stream-extensions
           :chunked-input-stream-trailers
           :chunked-io-stream
           :chunked-output-stream
           :chunked-stream
           :chunked-stream-input-chunking-p
           :chunked-stream-output-chunking-p
           :chunked-stream-stream
           :input-chunking-body-corrupted
           :input-chunking-unexpected-end-of-file
           :make-chunked-stream
           :read-http-headers
           :peek-char*
           :read-char*
           :read-line*
           :read-name-value-pair
           :read-name-value-pairs
           :read-token
           :skip-whitespace
           :syntax-error
           :token-char-p
           :trim-whitespace
           :with-character-stream-semantics))

