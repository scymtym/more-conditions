;;;; types.lisp --- Types used in the more-conditions system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)

;;; type `reference-spec'

(deftype reference-spec ()
  "A documentation reference of the form

     (DOCUMENT PART [LINK])

   where DOCUMENT is a keyword, PART is a string or list of strings
   and LINK, if present, is a string."
  '(cons keyword                      ; document
         (cons (or string list)       ; part
               (cons (or null string) ; link
                     null))))

(defun reference-document (spec)
  "Return the document of SPEC."
  (first spec))

(defun reference-part (spec)
  "Return the part of SPEC."
  (second spec))

(defun reference-link (spec)
  "Return the link of SPEC."
  (third spec))

;;; progress related types

(deftype progress-designator ()
  "Values describe progress of an operation.

   nil

     Progress is not known

   t

     Task has been completed.

   real

     Completion percentage as a real number between 0 (no progress)
     and 1 (completed; note that t should be used in this case, at
     least in a subsequently signaled condition)."
  '(or null (eql t) (real 0 1)))

(defun progress->real (progress)
  (declare (type progress-designator progress))
  (case progress
    ((nil)  0)
    ((t)    1)
    (t      progress)))
