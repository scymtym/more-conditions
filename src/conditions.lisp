;;;; conditions.lisp --- Conditions provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)


;;; Generic condition utilities
;;

(defgeneric cause (condition)
  (:method ((condition condition))
    nil)
  (:documentation
   "Return the condition that was signaled and caused CONDITION to be
signaled."))

(defgeneric root-cause (condition)
  (:method ((condition condition))
    condition)
  (:documentation
   "Return the condition that was originally signaled and eventually
caused CONDITION to be signaled."))

(define-condition chainable-condition (condition)
  ((cause :initarg  :cause
	  :type     (or null condition)
	  :reader   cause
	  :initform nil
	  :documentation
	  "The condition which originally caused the condition to be
signaled."))
  (:documentation
   "Instances of this class can contain another condition instance
which originally caused the condition to be signaled. This structure
can continue recursively thus forming a chain of causing
conditions."))

(defmethod root-cause ((condition chainable-condition))
  (if-let ((cause (cause condition)))
    (root-cause cause)
    condition))

(defun maybe-print-cause (stream condition &optional colon? at?)
  "Print the condition that caused CONDITION to be signaled (if any)
onto STREAM."
  (declare (ignore colon? at?))
  (format stream "~@[ ~_Caused by:~&~@<> ~@;~A~@:>~]"
	  (cause condition)))

(defun maybe-print-explanation (stream condition &optional colon? at?)
  "Format the message contained in the `simple-condition' CONDITION on
STREAM.

When COLON? is non-nil, the explanation is printed in an indented
logical block."
  (declare (ignore at?))

  (if (simple-condition-format-control condition)
      (progn
	(format stream ": ~_")
	(pprint-logical-block (stream nil :per-line-prefix (if colon?
							       "  " ""))
	  (apply #'format stream
		 (simple-condition-format-control   condition)
		 (simple-condition-format-arguments condition))))
      (write-char #\. stream)))


;;; Program error conditions
;;

(define-condition missing-required-argument (program-error)
  ((parameter :initarg  :parameter
	      :type     symbol
	      :reader   missing-required-argument-parameter
	      :documentation
	      "The parameter for which a value should have been
supplied."))
  (:report
   (lambda (condition stream)
     (format stream "~@<No value has been supplied for the required ~
parameter ~S.~@:>"
	     (missing-required-argument-parameter condition))))
  (:documentation
   "This error is signaled when no value is supplied for a required
parameter."))

(defun missing-required-argument (parameter)
  "Signal a `missing-required-argument' error for PARAMETER."
  (error 'missing-required-argument
	 :parameter parameter))

(define-condition incompatible-arguments (program-error)
  ((parameters :initarg  :parameters
	       :type     list
	       :reader   incompatible-arguments-parameters
	       :documentation
	       "A list of the parameters for which incompatible values
have been supplied.")
   (values     :initarg  :values
	       :type     list
	       :reader   incompatible-arguments-values
	       :documentation
	       "A list of the incompatible values."))
  (:report
   (lambda (condition stream)
     (let ((parameters (incompatible-arguments-parameters condition))
	   (values     (incompatible-arguments-values     condition)))
      (format stream "~@<~:[No arguments are~;~:*The combination of ~
arguments~&~/more-conditions::print-arguments/~2&is~] invalid.~:>"
	      (when parameters (list parameters values))))))
  (:documentation
   "This error is signaled when an incompatible combination of
arguments is supplied."))

(defun incompatible-arguments (&rest arguments)
  "Signal an `incompatible-arguments' error for ARGUMENTS which has to
be of the form

  PARAMETER1 VALUE1 PARAMETER2 VALUE2 ..."
  (let ((parameters (loop :for parameter :in arguments :by #'cddr
		       :collect parameter))
	(values     (loop :for value :in (rest arguments) :by #'cddr
		       :collect value)))
    (error 'incompatible-arguments
	   :parameters parameters
	   :values     values)))


;;; Initarg errors
;;

(define-condition initarg-error (program-error)
  ((class :initarg  :class
	  :type     symbol
	  :reader   initarg-error-class
	  :documentation
	  "The class for which the initarg error occurred."))
  (:report
   (lambda (condition stream)
     (format stream "~@<Invalid initargs have been supplied for class ~
~S.~@:>"
	     (initarg-error-class condition))))
  (:documentation
   "This error is signaled when invalid initargs are supplied."))

(define-condition missing-required-initarg (missing-required-argument
					    initarg-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "~@<The initarg ~S is required by class ~S, but ~
has not been supplied.~@:>"
	     (missing-required-argument-parameter condition)
	     (initarg-error-class                 condition))))
  (:documentation
   "This error is signaled when an initarg that is required by a class
is not supplied."))

(defun missing-required-initarg (class initarg)
  "Signal a `missing-required-initarg' error for CLASS and INITARG."
  (error 'missing-required-initarg
	 :parameter initarg
	 :class     class))

(define-condition incompatible-initargs (incompatible-arguments
					 initarg-error)
  ()
  (:report
   (lambda (condition stream)
     (let ((parameters (incompatible-arguments-parameters condition))
	   (values     (incompatible-arguments-values     condition)))
       (format stream "~@<~:[No initargs are~;~:*The combination of ~
initargs~&~/more-conditions::print-arguments/~2&is~] invalid for class ~S.~:>"
	      (when parameters (list parameters values))
	      (initarg-error-class condition)))))
  (:documentation
   "This error is signaled when incompatible initargs are supplied."))

(defun incompatible-initargs (class &rest initargs)
  "Signal an `incompatible-initargs' error for CLASS and INITARGS."
  (let ((parameters (loop :for parameter :in initargs :by #'cddr
		       :collect parameter))
	(values     (loop :for value :in (rest initargs) :by #'cddr
		       :collect value)))
    (error 'incompatible-initargs
	   :parameters parameters
	   :values     values
	   :class      class)))


;;; Utility functions
;;

(defun print-arguments (stream parameters-and-values &optional at? colon?)
  "Print PARAMETERS-AND-VALUES which has to be of the form

  (PARAMETERS VALUES)

onto STREAM. AT? and COLON? are ignored."
  (declare (ignore at? colon?))
  (destructuring-bind (parameters values) parameters-and-values
    (let ((max-name-length
	   (reduce #'max parameters
		   :key           (compose #'length #'prin1-to-string)
		   :initial-value 0)))
      (format stream "~:{~,,1<~%~2@T~VS~@;~S~>~}"
	      (mapcar #'list
		      (circular-list max-name-length) parameters values)))))
