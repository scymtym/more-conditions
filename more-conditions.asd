;;;; more-conditions.asd --- System definition for more-conditions.
;;;;
;;;; Copyright (C) 2010, 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:more-conditions-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:more-conditions-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 4
  "Minor component of version number.")

(defparameter +version-revision+ 2
  "Revision component of version number.")

(defun version/list ()
  "Return a version of the form (MAJOR MINOR REVISION)."
  (list +version-major+ +version-minor+ +version-revision+))

(defun version/string ()
  "Return a version string of the form \"MAJOR.MINOR.REVISION\"."
  (format nil "~{~A.~A.~A~}" (version/list)))

;;; System definition

(defsystem :more-conditions
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "This system provides some generic condition classes in
                conjunction with support functions and macros."
  :long-description "The idea is similar to
                     `alexandria:required-argument' but more
                     fine-grained.

                     In addition, there is support for translating
                     conditions at layer boundaries in larger systems."
  :depends-on  (:alexandria)
  :encoding    :utf-8
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "variables")
                              (:file       "protocol")
                              (:file       "conditions")
                              (:file       "macros")))

                (:static-file "README.org"))

  :in-order-to ((test-op (test-op :more-conditions-test))))

(defsystem :more-conditions-test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "LLGPLv3; see COPYING file for details."
  :description "Unit tests for the more-conditions system."
  :depends-on  ((:version :let-plus        "0.2")
                (:version :more-conditions #.(version/string))
                (:version :fiveam          "1.1"))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "macros")))))

(defmethod perform ((op        test-op)
                    (component (eql (find-system :more-conditions-test))))
  (uiop:symbol-call '#:more-conditions.test "RUN-TESTS"))
