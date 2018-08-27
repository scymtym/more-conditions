;;;; more-conditions.asd --- System definition for more-conditions.
;;;;
;;;; Copyright (C) 2010-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :more-conditions
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "This system provides some generic condition classes in
                conjunction with support functions and macros."
  :long-description "The idea is similar to
                     `alexandria:required-argument' but more
                     fine-grained.

                     In addition, there is support for translating
                     conditions at layer boundaries in larger systems."
  :depends-on  (:alexandria
                (:version :closer-mop "1.0.0"))
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

  :in-order-to ((test-op (test-op :more-conditions/test))))

(defsystem :more-conditions/test
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     (:read-file-form "version-string.sexp")
  :license     "LLGPLv3" ; see COPYING file for details.
  :description "Unit tests for the more-conditions system."
  :depends-on  ((:version :let-plus        "0.2")
                (:version :more-conditions (:read-file-form "version-string.sexp"))
                (:version :fiveam          "1.3"))
  :encoding    :utf-8
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "macros")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :more-conditions/test))))
  (uiop:symbol-call '#:more-conditions.test '#:run-tests))
