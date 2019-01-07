;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Base: 10 -*-

(in-package :chunga)

(define-condition chunga-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to Chunga."))

(define-condition chunga-error (chunga-condition stream-error)
  ()
  (:documentation "Superclass for all errors related to Chunga.  This
is a subtype of STREAM-ERROR, so STREAM-ERROR-STREAM can be used to
access the offending stream."))

(define-condition chunga-simple-error (chunga-error simple-condition)
  ()
  (:documentation "Like CHUNGA-ERROR but with formatting capabilities."))

(define-condition parameter-error (chunga-simple-error)
  ()
  (:documentation "Signalled if a function was called with
inconsistent or illegal parameters."))

(define-condition syntax-error (chunga-simple-error)
  ()
  (:documentation "Signalled if Chunga encounters wrong or unknown
syntax when reading data."))

(define-condition chunga-warning (chunga-condition warning)
  ()
  (:documentation "Superclass for all warnings related to Chunga."))

(define-condition chunga-simple-warning (chunga-warning simple-condition)
  ()
  (:documentation "Like CHUNGA-WARNING but with formatting capabilities."))

(define-condition input-chunking-unexpected-end-of-file (chunga-error)
  ()
  (:documentation "A condition of this type is signaled if we reach an
unexpected EOF on a chunked stream with input chunking enabled."))

(define-condition input-chunking-body-corrupted (chunga-error)
  ((last-char :initarg :last-char
              :documentation "The \(unexpected) character which was read.")
   (expected-chars :initarg :expected-chars
                   :documentation "The characters which were expected.
A list of characters or one single character."))
  (:report (lambda (condition stream)
             (with-slots (last-char expected-chars)
                 condition
               (format stream "Chunked stream ~S seems to be corrupted.
Read character ~S, but expected ~:[a member of ~S~;~S~]."
                       (stream-error-stream condition)
                       last-char (atom expected-chars) expected-chars))))
  (:documentation "A condition of this type is signaled if an
unexpected character \(octet) is read while reading from a chunked
stream with input chunking enabled."))
