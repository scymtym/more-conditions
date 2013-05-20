;;;; conditions.lisp --- Conditions provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)

;;; Generic condition utilities

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
  (let ((*print-references* (eq *print-references* :force)))
    (format stream "~@[ ~_Caused by:~&~@<> ~@;~A~@:>~]"
            (cause condition))))

(defun maybe-print-explanation (stream condition &optional colon? at?)
  "Format the message contained in the `simple-condition' CONDITION on
STREAM.

If CONDITION does not have a message, print \".\". This is intended
for messages which can be either

     \"MESSAGE.\"
  or \"MESSAGE: EXPLANATION\".

When COLON? is non-nil, the explanation is printed in an indented
logical block.

When AT? is non-nil and CONDITION does not have an explanation,
suppress printing \".\"."
  (cond
    ((simple-condition-format-control condition)
     (format stream ": ~_")
     (pprint-logical-block (stream nil :per-line-prefix (if colon?
                                                            "  " ""))
       (apply #'format stream
              (simple-condition-format-control   condition)
              (simple-condition-format-arguments condition))))
    ((not at?)
     (write-char #\. stream))))

;;; Program error conditions

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
                      arguments~&~/more-conditions:print-arguments/~2&is~] ~
                      invalid.~:>"
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
                       initargs~&~/more-conditions:print-arguments/~2&is~] ~
                       invalid for class ~S.~:>"
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

;;; Class `reference-condition'

(defun print-reference (stream spec &optional at? colon?)
  "Print reference SPEC onto STREAM.
AT? and COLON? are ignored."
  (declare (ignore at? colon?))
  (with-accessors ((document reference-document)
                   (part     reference-part)
                   (link     reference-link)) spec
    (format stream "~A, ~{~A~^ » ~}~@[ <~A>~]"
            document (ensure-list part) link)))

(defgeneric condition-references (condition)
  (:documentation
   "Return a list of references (of type `reference-spec') which are
associated to CONDITION."))

(defmethod condition-references ((condition t))
  "Return nil since arbitrary objects do not have references
associated to them."
  nil)

(defmethod condition-references :around ((condition chainable-condition))
  "Merge references associated to CONDITION with those associated to
the transitive causes of CONDITION."
  (remove-duplicates
   (append (when-let ((cause (cause condition)))
             (condition-references cause))
           (call-next-method))
   :test     #'equal
   :from-end t))

;; Note: based on identically named class in SBCL's
;; src/code/condition.lisp
(define-condition reference-condition (condition)
  ((references :initarg  :references
               :type     list ; of `reference-spec'
               :reader   condition-references
               :initform '()
               :documentation
               "Stores a list of references of type
`reference-spec'."))
  (:documentation
   "This condition class is intended to be mixed into condition
classes which can associate documentation references to their
instances."))

(defmethod print-object :after ((object reference-condition) stream)
  (when (and (not *print-escape*) (not *print-readably*)
             *print-references*
             (condition-references object))
    (format stream "~&See also:~%~<  ~@;~
                    ~{~/more-conditions:print-reference/~^~%~}~:>"
            (list (condition-references object)))))

;;; Progress conditions

(defgeneric progress-condition-message (condition)
  (:documentation
   "Return a string describing CONDITION or NULL"))

(defmethod progress-condition-message ((condition condition))
  nil)

(define-condition progress-condition (condition)
  ((operation :initarg  :operation
              :type     symbol
              :reader   progress-condition-operation
              :initform nil
              :documentation
              "Stores a symbol identifying the operation for which the
condition reports progress.")
   (progress  :initarg  :progress
              :type     progress-designator
              :accessor progress-condition-progress
              :initform nil
              :documentation
              "Stores the operation progress indicated by the
condition. See type `progress-designator'."))
  (:documentation
   "This condition is signaled to indicate the progress of execution
of an operation during the execution of that operation

Note that this condition does not have to be handled and its signaling
usually does not lead to a transfer of control."))

(defmethod print-object ((object progress-condition) stream)
  (flet ((do-it ()
           (format stream "~@<~@[~A: ~
                           ~]~/more-conditions:print-progress-percentage/~:>"
                   (progress-condition-operation object)
                   (progress-condition-progress object))))
    (if *print-escape*
        (print-unreadable-object (object stream :type t :identity t)
          (do-it))
        (do-it))))

(define-condition simple-progress-condition (progress-condition
                                             simple-condition)
  ()
  (:documentation
   "Like `progress-condition' but supports format control and format
arguments to produce a report to go along with the raw progress
information."))

(defmethod progress-condition-message ((condition simple-progress-condition))
  (apply #'format nil
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))

(defmethod print-object ((object simple-progress-condition) stream)
  (call-next-method)
  (unless *print-escape*
    (maybe-print-explanation stream object nil t)))

(defun progress (&optional operation progress
                 format-control-or-condition-class
                 &rest format-arguments-or-initargs)
  "Signal a progress condition indicating completion status PROGRESS
for OPERATION.

As with `cl:signal', `cl:error' and `cl:warn',
FORMAT-CONTROL-OR-CONDITION-CLASS and FORMAT-ARGUMENTS-OR-INITARGS
either specify a condition class and initargs or a report format
control string and format arguments."
  (declare (type progress-designator progress))
  (if (stringp format-control-or-condition-class)
      (signal 'simple-progress-condition
              :operation        operation
              :progress         progress
              :format-control   format-control-or-condition-class
              :format-arguments format-arguments-or-initargs)
      (apply #'signal (or format-control-or-condition-class
                          'progress-condition)
             :operation operation
             :progress  progress
             format-arguments-or-initargs)))

(defun progressing (function operation
                    &optional
                    format-control-or-condition-class
                    &rest format-arguments-or-initargs)
  "Return a function which signals a progress condition for OPERATION
and calls FUNCTION.

As with `cl:signal', `cl:error' and `cl:warn',
FORMAT-CONTROL-OR-CONDITION-CLASS and FORMAT-ARGUMENTS-OR-INITARGS
either specify a condition class and initargs or a report format
control string and format arguments. However, if
FORMAT-CONTROL-OR-CONDITION-CLASS is nil, a format string which prints
all arguments passed to FUNCTION is used.

Example:

  (let ((items '(1 2 3 4 5)))
    (with-sequence-progress (:foo items)
      (mapcar (progressing #'1+ :foo \"Frobbing\") items)))"
  (if format-control-or-condition-class
      (lambda (&rest args)
        (apply #'progress operation nil
               format-control-or-condition-class
               format-arguments-or-initargs)
        (apply function args))
      (lambda (&rest args)
        (apply #'progress operation nil "~@{~A~^ ~}" args)
        (apply function args))))

;;; Utility functions

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

(defun print-progress-percentage (stream progress &optional colon? at?)
  (declare (ignore colon? at?)
           (type progress-designator progress))
  (format stream "~:[???.??~;~:*~6,2,2F~] %"
          (case progress
            ((nil) nil)
            ((t)   1)
            (t     progress))))
