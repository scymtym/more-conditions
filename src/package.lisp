;;;; package.lisp --- Package definition for the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:more-conditions
  (:use
   #:cl
   #:alexandria)

  ;; Types
  (:export
   #:reference-spec

   #:reference-document
   #:reference-part
   #:reference-link

   #:print-reference)

  ;; Variables
  (:export
   #:*print-references*)

  ;; Conditions
  (:export
   ;; simple condition utilities
   #:maybe-print-explanation

   ;; chaining of conditions
   #:cause
   #:root-cause

   #:maybe-print-cause

   #:chainable-condition

   ;; argument errors
   #:missing-required-argument           ; condition and function
   #:missing-required-argument-parameter

   #:incompatible-arguments              ; condition and function
   #:incompatible-arguments-parameters
   #:incompatible-arguments-values

   ;; initarg errors
   #:initarg-error
   #:initarg-error-class

   #:missing-required-initarg            ; condition and function

   #:incompatible-initargs               ; condition and function

   ;; references
   #:condition-references

   #:reference-condition)

  ;; Macros
  (:export
   #:with-condition-translation
   #:define-condition-translating-method

   #:error-behavior-restart-case)

  (:documentation
   "This package provides generic conditions and condition-related
utilities.

Conditions
* `chainable-condition'
* `missing-required-argument'
  * `missing-required-initarg'
* `incompatible-arguments'
  * `incompatible-initargs'
* `reference-condition'

Condition helper functions
* `maybe-print-cause'
* `maybe-print-explanation'
* `print-reference'

Macros
* `with-condition-translation'
* `define-condition-translating-method'

* `error-behavior-restart-cases'"))
