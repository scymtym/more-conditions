;;;; variables.lisp --- Variables used in the more-conditions system.
;;;;
;;;; Copyright (C) 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:more-conditions)

;;; `*print-references*' special variable

(declaim (special *print-references*))

(defvar *print-references* t
  "When non-nil, `reference-condition' instances print reference
   information in addition to the primary condition report.")
