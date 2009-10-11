;;; this is a -*- Lisp -*- file 

;;; This file is part of cl-netstrings.
;;; Copyright (C) 2008, Daniel Janus.

(defpackage #:netstrings-asd
  (:use #:cl #:asdf))

(in-package #:netstrings-asd)

(defsystem cl-netstrings
  :name "cl-netstrings"
  :version "0.1"
  :maintainer "Daniel Janus"
  :author "Daniel Janus"
  :licence "MIT"
  :description "A library for handling netstrings as described in http://cr.yp.to/proto/netstrings.txt."
  :depends-on (:iterate :arnesi)
  :components ((:module 
                src
                :components
                ((:file "netstrings")))))
