;;;; package.lisp --- Package definition for unit tests of the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:more-conditions.test
  (:use
   #:cl
   #:alexandria
   #:lift

   #:more-conditions)

  (:export
   #:root)

  (:documentation
   "This package contains unit tests for the more-conditions
    system."))

(cl:in-package #:more-conditions.test)

(deftestsuite root ()
  ()
  (:documentation
   "Root unit test suite for the more-conditions system."))
