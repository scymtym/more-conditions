;;;; protocol.lisp --- Protocol provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)

;;; Causing conditions

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

;;; Documentation references

(defgeneric condition-references (condition)
  (:method ((condition t))
    ;; Return nil since arbitrary objects do not have references
    ;; associated to them.
    nil)
  (:method :around ((condition t))
    ;; When references of CONDITION are specified in form of
    ;; functions, resolve them by calling the functions.
    (mappend (lambda (spec)
               (etypecase spec
                 (function       (funcall spec condition))
                 (reference-spec (list spec))))
             (call-next-method)))
  (:documentation
   "Return a list of references (of type `reference-spec') which are
    associated to CONDITION."))

(defgeneric direct-default-references (class)
  (:method ((class symbol))
    (direct-default-references (find-class class)))
  (:method ((class class))
    ;; Default behavior is extracting default references from the
    ;; default initargs of CLASS.
    (values (default-initarg-value class :direct-references)))
  (:documentation
   "Return a list of references (of type `reference-spec') which are
    the default references for CLASS (but not CLASSes superclasses;
    see `default-references')."))

(defgeneric default-references (class)
  (:method ((class symbol))
    (default-references (find-class class)))
  (:method ((class class))
    ;; Default behavior is collecting default references from the
    ;; superclass closure unless references have been explicitly
    ;; specified.
    (multiple-value-bind (references references?)
        (default-initarg-value class :references)
      (if (or (not references?) (eq references :compute))
          (append (direct-default-references class)
                  (mappend #'default-references
                           (closer-mop:class-direct-superclasses class)))
          references)))
  (:documentation
   "Return a list of references (of type `reference-spec') which are
    the default references for CLASS (and all of CLASSes superclasses;
    i.e. transitive `direct-default-references'-closure)."))

;;; Progress reporting

(defgeneric progress-condition-message (condition)
  (:method ((condition condition))
    nil)
  (:documentation
   "Return a string describing CONDITION or nil."))

;;; Utility functions

;; If CLASS has a default initarg entry for INITARG, return two
;; values: 1) the value 2) t. Return nil otherwise.
(defun default-initarg-value (class initarg)
  (when-let* ((defaults     (closer-mop:class-direct-default-initargs
                             class))
              (spec         (find initarg defaults :key #'first :test #'eq))
              (initfunction (third spec)))
    ;; When there is a default initarg for INITARG, SPEC is of the
    ;; form
    ;;
    ;;   (INITARG INITFORM INITFUNCTION)
    ;;
    ;; where INITFUNCTION, when called without arguments, returns
    ;; the initial value.
    (values (funcall initfunction) t)))
