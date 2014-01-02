;;;; protocol.lisp --- Protocol provided by the more-conditions system.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

;;; Progress reporting

(defgeneric progress-condition-message (condition)
  (:method ((condition condition))
    nil)
  (:documentation
   "Return a string describing CONDITION or nil."))
