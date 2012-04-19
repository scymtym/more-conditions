;;; package.lisp --- Package definition for the cl-more-conditions system.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:defpackage :more-conditions
  (:use
   :cl
   :alexandria)

  ;; Conditions
  (:export
   ;; simple condition utilities
   :maybe-print-explanation

   ;; chaining of conditions
   :cause
   :root-cause

   :maybe-print-cause

   :chainable-condition

   ;; argument errors
   :missing-required-argument           ;; condition and function
   :missing-required-argument-parameter

   :incompatible-arguments              ;; condition and function
   :incompatible-arguments-parameters
   :incompatible-arguments-values

   ;; initarg errors
   :initarg-error
   :initarg-error-class

   :missing-required-initarg            ;; condition and function

   :incompatible-initargs               ;; condition and function
   )

  ;; Macros
  (:export
   :with-condition-translation
   :define-condition-translating-method)

  (:documentation
   "This package provides generic conditions and condition-related
utilities.

Conditions
* `chainable-condition'
* `missing-required-argument'
  * `missing-required-initarg'
* `incompatible-arguments'
  * `incompatible-initargs'

Condition helper functions
* `maybe-print-cause'
* `maybe-print-explanation'

Macros
* `with-condition-translation'
* `define-condition-translating-method'"))
