;;;; types.lisp --- Types used in the more-conditions system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)


;;; type `reference-spec'
;;

(deftype reference-spec ()
  "A documentation reference of the form

  (DOCUMENT PART [LINK])

where DOCUMENT is a keyword, PART is a string or list of strings and
LINK, if present, is a string."
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
