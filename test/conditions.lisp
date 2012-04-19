;;; conditions.lisp --- Unit tests for conditions provided by the cl-more-conditions system.
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
functions provided by the cl-more-conditions system."))

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
