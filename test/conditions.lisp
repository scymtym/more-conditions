;;; conditions.lisp --- Unit tests for conditions provided by the more-conditions system.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :more-conditions.test)

(deftestsuite conditions-root (root)
  ()
  (:documentation
   "Root test suite for conditions and condition-related helper
functions provided by the more-conditions system."))

(deftestsuite maybe-print-cause-root (conditions-root)
  ()
  (:setup
   ;; Without eval, causes failed aver in SBCL.
   (eval
    `(define-condition foo-error (error
				  chainable-condition)
       ()
       (:report
	(lambda (condition stream)
	  (format stream "Foo-error occurred.~/more-conditions::maybe-print-cause/"
		  condition))))))
  (:documentation
   "Test suite for the `maybe-print-cause' helper function."))

(addtest (maybe-print-cause-root
          :documentation
	  "Test printing condition instances using the
`maybe-print-cause' helper function.")
  print

  (ensure-cases (initargs expected)
      `((nil
	 "Foo-error occurred.")
	((:cause ,(make-condition 'simple-error
				  :format-control "The number was ~S."
				  :format-arguments '(1)))
	 "Foo-error occurred. Caused by:
> The number was 1."))

    (ensure-same (princ-to-string
		  (apply #'make-condition 'foo-error initargs))
		 expected
		 :test #'string=)))

(deftestsuite maybe-print-explanation-root (conditions-root)
  ()
  (:setup
   ;; Without eval, causes failed aver in SBCL.
   (eval
    `(define-condition simple-foo-error (simple-error)
       ()
       (:report
	(lambda (condition stream)
	  (format stream "Foo-error occurred~/more-conditions::maybe-print-explanation/"
		  condition))))))
  (:documentation
   "Test suite for the `maybe-print-explanation' helper function."))

(addtest (maybe-print-explanation-root
          :documentation
	  "Test printing condition instances using the
`maybe-print-explanation' helper function.")
  print

  (ensure-cases (initargs expected)
      '((nil
	 "Foo-error occurred.")
	((:format-control   "the number was ~S."
	  :format-arguments (1))
	 "Foo-error occurred: the number was 1."))

    (ensure-same (princ-to-string
		  (apply #'make-condition 'simple-foo-error initargs))
		 expected
		 :test #'string=)))


;;; Program error conditions
;;

(defmacro define-condition-suite ((name) &body cases)
  (let ((suite-name (format-symbol *package* "~A-ROOT" name)))
   `(progn
      (deftestsuite ,suite-name (conditions-root)
	()
	(:documentation
	 ,(format nil "Unit tests for the `~(~A~)' condition class."
		  name)))

      (addtest (,suite-name
		:documentation
		,(format nil "Test printing instances of the `~(~A~)' condition class"
			 name))
	construct-and-print/make-condition

	(ensure-cases (initargs constructor-args expected) (list ,@cases)
	  (ensure-same (princ-to-string (apply #'make-condition ',name initargs))
		       expected
		       :test #'string=)))

      ,@(when (fboundp name)
	  `((addtest (,suite-name
		      :documentation
		      ,(format nil "Test printing instances of the `~(~A~)' condition class"
			       name))
	      construct-and-print/constructor

	      (ensure-cases (initargs constructor-args expected) (list ,@cases)
		(ensure-same (handler-case
				 (apply #',name constructor-args)
			       (error (condition)
				 (princ-to-string condition)))
			     expected
			     :test #'string=))))))))

(define-condition-suite (missing-required-argument)
  '((:parameter :foo)
    (:foo)
    "No value has been supplied for the required parameter :FOO."))

(define-condition-suite (incompatible-arguments)
  '((:parameters () :values ())
    ()
    "No arguments are invalid.")
  '((:parameters (:foo) :values (1))
    (:foo 1)
    "The combination of arguments

  :FOO 1

is invalid.")
  '((:parameters (:foo :barbar) :values (1 2))
    (:foo 1 :barbar 2)
    "The combination of arguments

  :FOO    1
  :BARBAR 2

is invalid."))

(define-condition-suite (initarg-error)
  '((:class :foo)
    nil
    "Invalid initargs have been supplied for class :FOO."))

(define-condition-suite (missing-required-initarg)
  '((:class :foo :parameter :bar)
    (:foo :bar)
    "The initarg :BAR is required by class :FOO, but has not been supplied."))

(define-condition-suite (incompatible-initargs)
  '((:class :foo :parameters () :values ())
    (:foo)
    "No initargs are invalid for class :FOO.")
  '((:class :foo :parameters (:bar) :values (1))
    (:foo :bar 1)
    "The combination of initargs

  :BAR 1

is invalid for class :FOO.")
  '((:class :foo :parameters (:bar :bazbaz) :values (1 2))
    (:foo :bar 1 :bazbaz 2)
    "The combination of initargs

  :BAR    1
  :BAZBAZ 2

is invalid for class :FOO."))
