;;; This file is part of cl-netstrings.
;;; Copyright (C) 2008, Daniel Janus.

(defpackage #:netstrings
  (:nicknames #:cl-netstrings)
  (:use #:cl #:iterate #:arnesi)
  (:export #:read-netstring #:write-netstring))

(in-package #:netstrings)

(defconstant +colon+ (char-code #\colon))
(defconstant +comma+ (char-code #\comma))

(defun digitp (byte) 
  "Checks whether BYTE represents a decimal digit in ASCII."
  (and (>= byte 48) (<= byte 57)))

(defun digitize (byte)
  "Converts a BYTE representing an ASCII decimal digit to that digit's
numeric value."
  (- byte 48))

(defun read-netstring (stream &optional (encoding :utf-8))
  "Reads a netstring from STREAM.  STREAM must be a binary input
stream that has an element type (UNSIGNED-BYTE 8).  After reading, the
sequence of bytes is converted to an ordinary lisp string using
ENCODING (see the documentation of ARNESI:OCTETS-TO-STRING for a list
of available encodings)."
  (let ((length 0)
        result)
    (iter (for char = (read-byte stream))
          (if (digitp char)
              (setf length (+ (* 10 length) (digitize char)))
              (if (eql char +colon+)
                  (finish)
                  (error "Colon required after length"))))
    (setf result (make-array length :element-type '(unsigned-byte 8)))
    (read-sequence result stream :end length)
    (unless (= (read-byte stream) +comma+)
      (error "Netstring not terminated by a comma"))
    (if encoding
        (octets-to-string result encoding)
        result)))
    
(defun write-netstring (string stream &optional (encoding :utf-8))
  "Writes a STRING to STREAM as a netstring.  STREAM must be a binary
output stream that has an element type (UNSIGNED-BYTE 8).  The string
is converted to a sequence of bytes using ENCODING and then written to
the stream.  See the documentation of ARNESI:OCTETS-TO-STRING for a
list of available encodings."
  (let ((data (if (stringp string)
                  (string-to-octets string encoding)
                  string)))
    (write-sequence (string-to-octets (princ-to-string (length data)) encoding) stream)
    (write-byte +colon+ stream)
    (write-sequence data stream)
    (write-byte +comma+ stream)))
