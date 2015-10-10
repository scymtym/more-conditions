;;;; package.lisp --- Package definition for unit tests of the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:more-conditions.test
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:fiveam

   #:more-conditions)

  (:export
   #:run-tests)

  (:documentation
   "This package contains unit tests for the more-conditions
    system."))

(cl:in-package #:more-conditions.test)

(def-suite :more-conditions
  :description
  "Unit test suite for the more-conditions system.")

(defun run-tests ()
  "Run tests of the more-conditions system."
  (let ((results (let ((*print-pretty* t))
                   (run :more-conditions))))
    (explain! results)
    (results-status results)))
