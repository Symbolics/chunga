;;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: CHUNGA; Base: 10 -*-

(in-package :chunga)

#-:lispworks
(defmacro when-let ((var expr) &body body)
  "Evaluates EXPR, binds it to VAR, and executes BODY if VAR has a true value."
  `(let ((,var ,expr))
     (when ,var ,@body)))

(defun ends-with-p (seq suffix &key (test #'char-equal))
  "Returns true if the sequence SEQ ends with the sequence
SUFFIX.  Individual elements are compared with TEST."
  (let ((mismatch (mismatch seq suffix :from-end t :test test)))
    (or (null mismatch)
        (= mismatch (- (length seq) (length suffix))))))

(defun make-keyword (string destructivep)
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Destructively modifies STRING if DESTRUCTIVEP is true."
  (intern (funcall
           (if destructivep
             (if (eq (readtable-case *readtable*) :upcase)
               #'nstring-upcase
               #'nstring-downcase)
             (if (eq (readtable-case *readtable*) :upcase)
               #'string-upcase
               #'string-downcase))
           string)
          :keyword))

;;; Genera uses a non-standard character set where the control characters
;;; have codepoints above 127.  Add the appropriate translations in READ-CHAR*
#+:genera
(defconstant +external-to-internal-codepoints+
  (let ((table (make-hash-table)))
    (setf (gethash #o010 table) #.(char-code #\Backspace))
    (setf (gethash #o011 table) #.(char-code #\Tab))
    (setf (gethash #o012 table) #.(char-code #\Linefeed))
    (setf (gethash #o014 table) #.(char-code #\Page))
    (setf (gethash #o015 table) #.(char-code #\Return))
    (setf (gethash #o177 table) #.(char-code #\Rubout))
    table))

(defun read-char* (stream &optional (eof-error-p t) eof-value)
  "The streams we're dealing with are all binary with element type
\(UNSIGNED-BYTE 8) and we're only interested in ISO-8859-1, so we use
this to `simulate' READ-CHAR."
  (cond (*char-buffer*
         (prog1 *char-buffer*
           (setq *char-buffer* nil)))
        (t
         ;; this assumes that character codes are identical to Unicode code
         ;; points, at least for Latin1
         (let ((char-code (read-byte stream eof-error-p eof-value)))
           (and char-code
                #-:genera (code-char char-code)
		#+:genera (code-char (gethash char-code +external-to-internal-codepoints+
					      char-code)))))))

(defun unread-char* (char)
  "Were simulating UNREAD-CHAR by putting the character into
*CHAR-BUFFER*."
  ;; no error checking, only used internally
  (setq *char-buffer* char)
  nil)
  
(defun peek-char* (stream &optional eof-error-p eof-value)
  "We're simulating PEEK-CHAR by reading a character and putting it
into *CHAR-BUFFER*."
  ;; no error checking, only used internally  
  (setq *char-buffer* (read-char* stream eof-error-p eof-value)))

(defmacro with-character-stream-semantics (&body body)
  "Binds *CHAR-BUFFER* around BODY so that within BODY we can use
READ-CHAR* and friends \(see above) to simulate a character stream
although we're reading from a binary stream."
  `(let ((*char-buffer* nil))
     ,@body))
